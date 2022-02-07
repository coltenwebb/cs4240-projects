{-# LANGUAGE FlexibleContexts #-}

module IR.Reader where

import Control.Monad (unless, void, when)
import qualified Data.Map as M
import qualified Data.Set as S
import IR.Function
import IR.Instruction
import IR.Program
import IR.Type
import System.IO (readFile)
import Text.Parsec
import Text.Parsec.Char ()
import Text.Parsec.Language
import Text.Parsec.Token (GenTokenParser (comma))
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Prim (parseFromFile)

type VariableMap = M.Map VariableName Type

type FunctionSet = S.Set FunctionName

type LabelSet = S.Set LabelName

data ParseState = ParseState
  {varMap :: VariableMap, funcSet :: FunctionSet, labelSet :: LabelSet}
  deriving (Show)

type Parsec' = Parsec String ParseState

lexer = Tok.makeTokenParser emptyDef

-- TODO:
--  1) Allow trailing whitespace
readProgramFile :: FilePath -> IO (Either ParseError Program)
readProgramFile fp = do
  input <- readFile fp
  return (runParser parseProgram initParseState fp input)

intrinsicFuncs :: S.Set FunctionName
intrinsicFuncs =
  S.fromList . map FunctionName $
    ["geti", "getf", "getc", "puti", "putf", "putc"]

initParseState :: ParseState
initParseState = ParseState mempty intrinsicFuncs mempty

insertVariable :: Variable -> Parsec' ()
insertVariable (Variable name tp) = do
  s <- getState
  let vm = varMap s
  case M.lookup name vm of
    Just var -> error $ "Redefinition of variable `" ++ show var ++ "`"
    Nothing -> putState $ s {varMap = M.insert name tp vm}

insertFunction :: FunctionName -> Parsec' ()
insertFunction fn = do
  s <- getState
  let fs = funcSet s
  if fn `S.member` fs
    then error $ "Redefinition of function `" ++ show fs ++ "`"
    else putState $ s {funcSet = fn `S.insert` fs}

insertLabel :: LabelName -> Parsec' ()
insertLabel label = do
  s <- getState
  let ls = labelSet s
  if label `S.member` ls
    then error $ "Redefinition of label `" ++ show label ++ "`"
    else putState $ s {labelSet = label `S.insert` ls}

parseStartFunc :: Parsec' ()
parseStartFunc = do
  string "#start_function"
  ps <- getState
  -- function declarations are global
  putState $ ps {varMap = mempty, labelSet = mempty}

parseEndFunc :: Parsec' ()
parseEndFunc = void (string "#end_function")

parseFunctionSignature :: Parsec' (Type, FunctionName, [Variable])
parseFunctionSignature = do
  tp <- parseType
  spaces
  name <- FunctionName <$> parseIdentifier
  char '('
  params <- parseParameter `sepBy` commaSep
  string "):"

  insertFunction name

  return (tp, name, params)

parseIdentifier :: Parsec' String
parseIdentifier =
  notFollowedBy digit >> many1 (alphaNum <|> char '_')
    <?> "identifier (var name, function name, etc.)"

parseType :: Parsec' Type
parseType =
  parseIntType
    <|> parseFloatType
    <|> parseVoidType
  where
    parseIntType = do
      string "int"
      parseArray IntType <|> return IntType

    parseFloatType = do
      string "float"
      parseArray FloatType <|> return FloatType

    parseVoidType = string "void" >> return VoidType

    parseArray tp = do
      char '['
      sz <- read <$> many1 digit
      char ']'
      return $ ArrayType (ArraySize sz) tp

parseParameter :: Parsec' Variable
parseParameter = do
  tp <- parseType
  spaces
  name <- VariableName <$> parseIdentifier
  let var = Variable name tp
  return var

parseVariable :: Type -> Parsec' Variable
parseVariable tp = do
  name <- VariableName <$> parseIdentifier
  array name <|> nonArray name
  where
    nonArray n = return $ Variable n tp
    array n = do
      char '['
      count <- read <$> many1 digit
      char ']'
      let var = Variable n (ArrayType (ArraySize count) tp)
      return var

spacesNoNewline :: Parsec' ()
spacesNoNewline = skipMany $ notFollowedBy newline >> space

parseVariableLists :: Parsec' [Variable]
parseVariableLists = do
  v1 <- i
  skipMany1 newline
  v2 <- f
  return $ v1 ++ v2
  where
    k t = parseVariable t `sepBy` commaSep
    i = string "int-list:" >> spacesNoNewline >> k IntType
    f = string "float-list:" >> spacesNoNewline >> k FloatType

commaSep :: Parsec' ()
commaSep = spacesNoNewline >> char ',' >> spacesNoNewline

parseFunction :: Parsec' Function
parseFunction = do
  parseStartFunc

  skipMany1 newline
  (tp, fname, params) <- parseFunctionSignature

  skipMany1 newline
  vars <- parseVariableLists

  mapM_ insertVariable params
  mapM_ insertVariable vars

  skipMany1 newline
  ins <- (spacesNoNewline >> parseInstruction) `sepEndBy` skipMany1 newline

  -- last newline handled by the `endBy` in `sepEndBy`
  parseEndFunc
  return $ Function fname tp params vars ins

parseProgram :: Parsec' Program
parseProgram = Program <$> parseFunction `sepEndBy` skipMany1 newline

parseInstruction :: Parsec' Instruction
parseInstruction =
  try labelOp <|> do
    ln <- LineNumber . sourceLine <$> getPosition
    op <- parseOpCode
    commaSep
    operands <- case op of
      ASSIGN -> try assignmentOp <|> arrayAssignOp
      ADD -> binaryOp
      SUB -> binaryOp
      MULT -> binaryOp
      DIV -> binaryOp
      AND -> binaryOp
      OR -> binaryOp
      GOTO -> gotoOp
      BREQ -> branchOp
      BRNEQ -> branchOp
      BRLT -> branchOp
      BRGT -> branchOp
      BRLEQ -> branchOp
      BRGEQ -> branchOp
      RETURN -> returnOp
      CALL -> callOp
      CALLR -> callrOp
      ARRAY_STORE -> arrayStoreOp
      ARRAY_LOAD -> arrayLoadOp
      -- Edge case since labels are represented as a pseudo-opcode,
      -- handled with `labelOp`
      LABEL -> error "Invalid opcode `label`"

    return $ Instruction op operands ln
  where
    labelOp = do
      spaces
      name <- LabelName <$> parseIdentifier
      char ':'

      ln <- LineNumber . sourceLine <$> getPosition

      return $ Instruction LABEL [LabelOperand name] ln

    -- Section 7.1
    assignmentOp = do
      o1 <- parseConstOrVarOperand
      commaSep
      o2 <- parseConstOrVarOperand

      -- "The first operand must be a variable."
      unless (isVariableOperand o1) $
        error $
          "Expected variable operand for assignment op, instead got `" ++ show o1 ++ "`"

      -- "The operands must be of the same basic type."
      unless (basicTypeMatches o1 o2) $
        error $
          "Non-matching basic types for constant operands `" ++ show o1
            ++ "` and `"
            ++ show o2
            ++ "`"

      return [o1, o2]

    -- Section 7.2
    binaryOp = do
      o1 <- parseConstOrVarOperand
      commaSep
      o2 <- parseConstOrVarOperand
      commaSep
      o3 <- parseConstOrVarOperand

      -- "The first operand must be a variable."
      unless (isVariableOperand o1) $
        error $
          "Expected variable operand for binary op, instead got `" ++ show o1 ++ "`"

      -- "The operands must be of the same basic type."
      unless (basicTypeMatches o1 o2 && basicTypeMatches o2 o3) $
        error $
          "Non-matching basic types for constant operands `" ++ show o1
            ++ "`, `"
            ++ show o2
            ++ "`, and `"
            ++ show o3
            ++ "`"

      return [o1, o2, o3]

    -- Section 7.3
    gotoOp = do
      o1 <- LabelOperand . LabelName <$> parseIdentifier
      return [o1]

    -- Section 7.4
    branchOp = do
      o1 <- LabelOperand . LabelName <$> parseIdentifier
      commaSep
      o2 <- parseConstOrVarOperand
      commaSep
      o3 <- parseConstOrVarOperand

      -- "The last two operands must be of the same basic type."
      unless (basicTypeMatches o2 o3) $
        error $
          "Non-matching basic types in branch op `" ++ show o2
            ++ "` and `"
            ++ show o3
            ++ "`"

      return [o1, o2, o3]

    -- Section 7.5
    returnOp = do
      o1 <- parseConstOrVarOperand

      -- "The operand must be of a basic type. There is no empty return instruction.
      -- A function with no return value should not contain a return instruction."
      unless (operandIsBasicType o1) $
        error $
          "Unexpected non-basic type in return op `" ++ show o1 ++ "`"

      return [o1]

    -- Section 7.6
    callOp = do
      func <- parseFunctionOperand

      args <-
        try
          ( do
              commaSep
              (spaces >> parseConstOrVarOperand) `sepBy` char ','
          )
          <|> return [] -- in the case of no args
      return (func : args)

    -- Section 7.7
    callrOp = do
      ret <- parseVariableOperand

      commaSep
      func <- parseFunctionOperand

      args <-
        try
          ( do
              commaSep
              (spaces >> parseConstOrVarOperand) `sepBy` char ','
          )
          <|> return [] -- in the case of no args
      return (ret : func : args)

    -- Section 7.8
    arrayStoreOp = do
      -- Differs from array_load in that first operand can be const.
      op1 <- parseConstOrVarOperand

      commaSep
      arr <- parseVariableOperand

      commaSep
      index <- parseConstOrVarOperand

      -- "The type of the third operand must be int."
      unless (operandIsIntBasicType index) $
        error $
          show index ++ " is not of basic type int"

      -- "The second operand must be an array."
      unless (operandIsArrayType arr) $ error $ show arr ++ " is not an array."

      -- "The type of the first operand must be the same as the element"
      -- "type of the second operand."
      unless (arrayElementTypeMatches arr op1) $
        error $
          "Cannot store value of type " ++ show (getType op1)
            ++ " to an array of type "
            ++ show (elemType (getType arr))

      return [op1, arr, index]

    -- Section 7.9
    arrayLoadOp = do
      -- "The first operand must be a variable."
      val <- parseVariableOperand

      commaSep
      arr <- parseVariableOperand

      commaSep
      index <- parseConstOrVarOperand

      -- "The type of the third operand must be int."
      unless (operandIsIntBasicType index) $
        error $
          show index ++ " is not of basic type int"

      -- "The second operand must be an array."
      unless (operandIsArrayType arr) $ error $ show arr ++ " is not an array."

      -- "The type of the first operand must be the same as the element"
      -- "type of the second operand."
      unless (arrayElementTypeMatches arr val) $
        error $
          "Cannot store value of type " ++ show (getType val)
            ++ " to an array of type "
            ++ show (elemType (getType arr))

      -- NOTE: Not sure if it's our job to check for index errors. Is that interpreter's job?
      -- when (arrayIndexOutofBound arr i) $ error $ "Array index out of bound: " ++ show arr

      return [val, arr, index]

    -- Section 7.10
    arrayAssignOp = do
      arr <- parseVariableOperand

      commaSep
      count <- parseIntOperand

      commaSep
      val <- parseIntOperand

      unless (operandIsArrayType arr) $ error $ show arr ++ " is not an array."

      -- Not our job? for now
      -- when (arrayIndexOutofBound arr count) $ error $ "Array index out of bound: " ++ show arr

      unless (basicTypeMatches arr val) $
        error $
          "Cannot store value of type " ++ show (getType val)
            ++ " to an array of type "
            ++ show (elemType (getType arr))

      return [arr, count, val]

parseInt :: Parsec' Int
parseInt = read <$> many1 digit

genOpCodeParser :: OpCode -> Parsec' OpCode
genOpCodeParser op =
  try (string (show op))
    >> lookAhead (spaces >> char ',')
    >> return op

parseOpCode :: Parsec' OpCode
parseOpCode = foldl1 (<|>) $ fmap genOpCodeParser [minBound .. maxBound]

parseConstantOperand :: Parsec' Operand
parseConstantOperand =
  try parseFloatOperand
    <|> parseIntOperand
    <?> "constant operand"

parseIntOperand :: Parsec' Operand
parseIntOperand = do
  val <- many1 (char '-' <|> digit)
  return (ConstantOperand (ConstantValue val) IntType)

parseFloatOperand :: Parsec' Operand
parseFloatOperand = do
  val <- many1 (char '-' <|> digit)
  char '.'
  precision <- many digit
  return (ConstantOperand (ConstantValue (val ++ "." ++ precision)) FloatType)

parseConstOrVarOperand :: Parsec' Operand
parseConstOrVarOperand =
  parseConstantOperand
    <|> parseVariableOperand
    <?> "constant or variable operand"

parseVariableOperand :: Parsec' Operand
parseVariableOperand = p <?> "variable operand"
  where
    p = do
      name <- VariableName <$> parseIdentifier
      t <- M.lookup name . varMap <$> getState
      case t of
        Nothing -> error $ "Undefined variable `" ++ show name ++ "`"
        Just tp -> return $ VariableOperand $ Variable name tp

parseFunctionOperand :: Parsec' Operand
parseFunctionOperand = do
  name <- FunctionName <$> parseIdentifier
  t <- S.member name . funcSet <$> getState
  if t
    then return $ FunctionOperand name
    else error $ "Undefined function `" ++ show name ++ "`"

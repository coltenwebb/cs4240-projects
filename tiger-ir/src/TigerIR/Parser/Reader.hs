{-# LANGUAGE FlexibleContexts #-}

module TigerIR.Parser.Reader where

import Control.Monad (unless, void, when)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified TigerIR.IrInstruction as T

import TigerIR.Parser.Legacy.Function
import TigerIR.Parser.Legacy.Instruction
import TigerIR.Parser.Legacy.Program
import TigerIR.Parser.Legacy.Type

import System.IO (readFile)
import Text.Parsec
import Text.Parsec.Char ()
import Text.Parsec.Language
import Text.Parsec.Token (GenTokenParser (comma))
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Prim (parseFromFile)

type VariableMap = M.Map VariableName Type
type Parsec' = Parsec String VariableMap

insertVariable :: Variable -> Parsec' ()
insertVariable (Variable name tp) = do
  vm <- getState
  case M.lookup name vm of
    Just tp -> error $ "Redefinition of variable `" ++ show name ++ "`"
    Nothing -> putState $ M.insert name tp vm

-- TODO:
--  1) Allow trailing whitespace / CRLF
--  2) Check labels, returns, main func, lnes 238-268 in IRReader.java
readProgramFile :: FilePath -> IO (Either ParseError Program)
readProgramFile fp = do
  input <- readFile fp
  return (runParser parseProgram mempty fp input)

parseStartFunc :: Parsec' ()
parseStartFunc = do
  string "#start_function"
  putState mempty

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
spacesNoNewline = skipMany $ notFollowedBy endOfLine >> space

parseVariableLists :: Parsec' [Variable]
parseVariableLists = do
  v1 <- i
  skipMany1 endOfLine
  v2 <- f

  return v1
  -- screw the floats in project 2
  --return $ v1 ++ v2
  where
    k t = parseVariable t `sepBy` commaSep
    i = string "int-list:" >> spacesNoNewline >> k IntType
    f = string "float-list:" >> spacesNoNewline >> k FloatType

commaSep :: Parsec' ()
commaSep = spacesNoNewline >> char ',' >> spacesNoNewline

parseFunction :: Parsec' Function
parseFunction = do
  parseStartFunc

  skipMany1 endOfLine
  (tp, fname, params) <- parseFunctionSignature

  skipMany1 endOfLine
  vars <- parseVariableLists

  mapM_ insertVariable params
  mapM_ insertVariable vars

  skipMany1 endOfLine
  ins <- (spacesNoNewline >> parseInstruction) `sepEndBy` skipMany1 endOfLine

  -- last newline handled by the `endBy` in `sepEndBy`
  parseEndFunc
  return $ Function fname tp params vars ins

parseProgram :: Parsec' Program
parseProgram = Program <$> parseFunction `sepEndBy` skipMany1 endOfLine

parseInstruction :: Parsec' Instruction
parseInstruction =
  try labelOp <|> do
    ln <- LineNumber . sourceLine <$> getPosition
    op <- parseOpCode
    commaSep
    _
    --case op of
    --  ASSIGN -> try arrayAssignOp <|> assignmentOp
    --  ADD -> binaryOp
    --  SUB -> binaryOp
    --  MULT -> binaryOp
    --  DIV -> binaryOp
    --  AND -> binaryOp
    --  OR -> binaryOp
    --  GOTO -> gotoOp
    --  BREQ -> branchOp
    --  BRNEQ -> branchOp
    --  BRLT -> branchOp
    --  BRGT -> branchOp
    --  BRLEQ -> branchOp
    --  BRGEQ -> branchOp
    --  RETURN -> returnOp
    --  CALL -> callOp
    --  CALLR -> callrOp
    --  ARRAY_STORE -> arrayStoreOp
    --  ARRAY_LOAD -> arrayLoadOp
    --  -- Edge case since labels are represented as a pseudo-opcode,
    --  -- handled with `labelOp`
    --  LABEL -> error "Invalid opcode `label`"

    --return $ Instruction op operands ln
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

      return [o1, o2]

    -- Section 7.2
    binaryOp = do
      o1 <- parseConstOrVarOperand
      commaSep
      o2 <- parseConstOrVarOperand
      commaSep
      o3 <- parseConstOrVarOperand

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

      return [o1, o2, o3]

    -- Section 7.5
    returnOp = do
      o1 <- parseConstOrVarOperand

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

      return [op1, arr, index]

    -- Section 7.9
    arrayLoadOp = do
      -- "The first operand must be a variable."
      val <- parseVariableOperand

      commaSep
      arr <- parseVariableOperand

      commaSep
      index <- parseConstOrVarOperand

      return [val, arr, index]

    -- Section 7.10
    arrayAssignOp = do
      arr <- parseVariableOperand

      commaSep
      count <- parseIntOperand

      commaSep
      val <- parseConstOrVarOperand

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
      t <- M.lookup name <$> getState
      case t of
        Nothing -> error $ "Undefined variable `" ++ show name ++ "`"
        Just tp -> return $ VariableOperand $ Variable name tp

parseFunctionOperand :: Parsec' Operand
parseFunctionOperand = do
  name <- FunctionName <$> parseIdentifier
  return $ FunctionOperand name

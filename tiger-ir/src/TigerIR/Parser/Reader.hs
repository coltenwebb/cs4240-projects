{-# LANGUAGE FlexibleContexts #-}

module TigerIR.Parser.Reader where

import Control.Monad (unless, void, when)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified TigerIR.IrInstruction as T
import qualified TigerIR.Types as T
import qualified TigerIR.Program as T

import TigerIR.Parser.Legacy.Function

-- We will be retuning the new TigerIr Instruction type instead
import TigerIR.Parser.Legacy.Instruction hiding (Instruction)
import TigerIR.Parser.Legacy.Program
import TigerIR.Parser.Legacy.Type
import qualified TigerIR.Parser.Legacy.Shim as Shim

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
readProgramFile :: FilePath -> IO (Either ParseError T.TigerIrProgram)
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

parseFunction :: Parsec' T.TigerIrFunction 
parseFunction = do
  parseStartFunc

  skipMany1 endOfLine
  (tp, (FunctionName fname), params) <- parseFunctionSignature

  skipMany1 endOfLine
  vars <- parseVariableLists

  mapM_ insertVariable params
  mapM_ insertVariable vars

  skipMany1 endOfLine
  ins <- (spacesNoNewline >> parseInstruction) `sepEndBy` skipMany1 endOfLine

  -- last newline handled by the `endBy` in `sepEndBy`
  parseEndFunc
  let tp' = returnsInt tp
  let params' = map var2Param params
  let vars' = map var2Local vars 
  -- let ins' = map T.IrInstruction ins

  return $ T.TigerIrFunction (T.FunctionName (T.Label fname)) tp' params' vars' ins 
  where 
    returnsInt IntType  = True
    returnsInt _        = False

    var2Param :: Variable -> T.ParamVar
    var2Param var@(Variable _ IntType) = (T.ParamV . Shim.v2v) var
    var2Param var@(Variable _ (ArrayType _ _)) = (T.ParamA . Shim.a2a) var
    var2Param _ = error "Invalid type when converting old variable type to new param type"

    var2Local :: Variable -> T.LocalVar
    var2Local var@(Variable _ IntType) = (T.LocalV . Shim.v2v) var
    var2Local var@(Variable _ (ArrayType _ _)) = (T.LocalA . Shim.a2a) var
    var2Local _ = error "Invalid type when converting old variable type to new param type"
    

parseProgram :: Parsec' T.TigerIrProgram
parseProgram = T.TigerIrProgram <$> parseFunction `sepEndBy` skipMany1 endOfLine

parseInstruction :: Parsec' T.TigerIrIns
parseInstruction =
  try labelOp <|> do
    ln <- T.LineNumber . sourceLine <$> getPosition
    op <- parseOpCode
    commaSep
    instr <- case op of
      ASSIGN -> try arrayAssignOp <|> assignmentOp
      -- Binary ops
      ADD  -> T.BinaryOperation T.Add <$> binaryOperands
      SUB  -> T.BinaryOperation T.Sub <$> binaryOperands
      MULT -> T.BinaryOperation T.Mult <$> binaryOperands
      DIV  -> T.BinaryOperation T.Div <$> binaryOperands
      AND  -> T.BinaryOperation T.And <$> binaryOperands
      OR   -> T.BinaryOperation T.Or <$> binaryOperands

      -- GOTO
      GOTO -> T.Goto <$> gotoOp

      -- Branch ops
      BREQ  -> T.BranchOperation T.Breq  <$> branchOperands
      BRNEQ -> T.BranchOperation T.Brneq <$> branchOperands
      BRLT  -> T.BranchOperation T.Brlt  <$> branchOperands
      BRGT  -> T.BranchOperation T.Brgt  <$> branchOperands
      BRLEQ -> T.BranchOperation T.Brleq <$> branchOperands
      BRGEQ -> T.BranchOperation T.Brgeq <$> branchOperands

      -- RETURN
      RETURN -> T.Return <$> returnOperand

      -- CALL
      CALL  -> callIns
      CALLR -> callInsR

      -- ARRAY Operations
      ARRAY_STORE -> T.ArrStore <$> arrayStoreOperands
      ARRAY_LOAD  -> T.ArrLoad  <$> arrayLoadOperands

      -- Edge case since labels are represented as a pseudo-opcode,
      -- handled with `labelOp`, not in the same format
      LABEL -> error "Invalid opcode `label`"

    return $ T.Instruction instr ln
  where
    labelOp = do
      spaces
      lab <- T.Label <$> parseIdentifier
      char ':'

      ln <- T.LineNumber . sourceLine <$> getPosition

      return $ T.Instruction (T.LabelIns lab) ln

    -- Section 7.1
    assignmentOp = do
      -- should be safe, because variable operand
      -- can only return variable operands lmao
      (VariableOperand dst) <- parseVariableOperand

      commaSep
      opr <- parseConstOrVarOperand

      oprndns <- case opr of
        VariableOperand v2
          -> return $ T.AssignVarOpsDV (Shim.v2v dst) (Shim.v2v v2)

        ConstantOperand c IntType
          -> return $ T.AssignVarOpsDI (Shim.v2v dst) (Shim.c2i c)

        _ -> error $ "unsupported 2nd argument to assign op: " ++ show opr

      return $ T.AssignVar oprndns

    -- Section 7.2
    binaryOperands = do
      (VariableOperand v1) <- parseVariableOperand
      commaSep
      o2 <- parseConstOrVarOperand
      commaSep
      o3 <- parseConstOrVarOperand

      case (o2, o3) of
        (VariableOperand v2, VariableOperand v3)
          -> return $ T.BinOpsDVV (Shim.v2v v1) (Shim.v2v v2) (Shim.v2v v3)

        (ConstantOperand c2 IntType, VariableOperand v3)
          -> return $ T.BinOpsDIV (Shim.v2v v1) (Shim.c2i c2) (Shim.v2v v3)

        (ConstantOperand c2 IntType, ConstantOperand c3 IntType)
          -> return $ T.BinOpsDII (Shim.v2v v1) (Shim.c2i c2) (Shim.c2i c3)

        (VariableOperand v2, ConstantOperand c3 IntType)
          -> return $ T.BinOpsDVI (Shim.v2v v1) (Shim.v2v v2) (Shim.c2i c3)

        _ -> fail $ "Unsupported binary operands: " ++ show o2 ++ " " ++ show o3


    -- Section 7.3
    gotoOp = T.Label <$> parseIdentifier

    -- Section 7.4
    branchOperands = do
      lab <- T.Label <$> parseIdentifier
      commaSep
      o2 <- parseConstOrVarOperand
      commaSep
      o3 <- parseConstOrVarOperand

      case (o2, o3) of
        (VariableOperand v2, VariableOperand v3)
          -> return $ T.BrOpsVV lab (Shim.v2v v2) (Shim.v2v v3)

        (ConstantOperand c2 IntType, VariableOperand v3)
          -> return $ T.BrOpsIV lab (Shim.c2i c2) (Shim.v2v v3)

        (ConstantOperand c2 IntType, ConstantOperand c3 IntType)
          -> return $ T.BrOpsII lab (Shim.c2i c2) (Shim.c2i c3)

        (VariableOperand v2, ConstantOperand c3 IntType)
          -> return $ T.BrOpsVI lab (Shim.v2v v2) (Shim.c2i c3)

        _ -> fail $ "Unsupported branch operands: " ++ show o2 ++ " " ++ show o3

    -- Section 7.5
    returnOperand = do
      o1 <- parseConstOrVarOperand

      case o1 of
        VariableOperand v         -> return $ T.Retvar (Shim.v2v v)
        ConstantOperand c IntType -> return $ T.Retimm (Shim.c2i c)
        _ -> fail $ "Unsupported return operand: " ++ show o1

    -- Section 7.6
    callIns = do
      fn <- parseFunctionOperand

      args <-
        try
          ( do
              commaSep
              (spaces >> parseConstOrVarOperand) `sepBy` char ','
          )
          <|> return [] -- in the case of no args

      return $ T.Call fn (Shim.legacyCallArgs2FnArgs args)

    -- Section 7.7
    callInsR = do
      -- This should be ok since parseVariableOperand
      -- can only return VarialbeOperands
      (VariableOperand dest) <- parseVariableOperand

      commaSep
      fn <- parseFunctionOperand

      args <-
        try
          ( do
              commaSep
              (spaces >> parseConstOrVarOperand) `sepBy` char ','
          )
          <|> return [] -- in the case of no args

      return $ T.Callr (Shim.v2v dest) fn (Shim.legacyCallArgs2FnArgs args)


    -- Section 7.8
    arrayStoreOperands = do
      -- Differs from array_load in that first operand can be const.
      src <- parseConstOrVarOperand

      commaSep
      -- This should be ok since parseVariableOperand
      -- can only return VarialbeOperands
      (VariableOperand arr) <- parseVariableOperand

      commaSep
      index <- parseConstOrVarOperand

      case (src, index) of
        (VariableOperand v, VariableOperand vix)
          -> return $ T.ArrStoreVAV (Shim.v2v v) (Shim.a2a arr) (Shim.v2v vix)

        (ConstantOperand c IntType, VariableOperand vix)
          -> return $ T.ArrStoreIAV (Shim.c2i c) (Shim.a2a arr) (Shim.v2v vix)

        (ConstantOperand c IntType, ConstantOperand cix IntType)
          -> return $ T.ArrStoreIAI (Shim.c2i c) (Shim.a2a arr) (Shim.c2i cix)

        (VariableOperand v, ConstantOperand cix IntType)
          -> return $ T.ArrStoreVAI (Shim.v2v v) (Shim.a2a arr) (Shim.c2i cix)

        _ -> error $
          "invalid args to array_store"
          ++ show [src, VariableOperand arr, index]

    -- Section 7.9
    arrayLoadOperands = do
      -- "The first operand must be a variable."
      (VariableOperand dst) <- parseVariableOperand

      commaSep
      (VariableOperand arr) <- parseVariableOperand

      commaSep
      index <- parseConstOrVarOperand

      case index of
        VariableOperand v -> return $
          T.ArrLoadDAV (Shim.v2v dst) (Shim.a2a arr) (Shim.v2v v)

        ConstantOperand c IntType -> return $
          T.ArrLoadDAI (Shim.v2v dst) (Shim.a2a arr) (Shim.c2i c)

        _ -> error $ "unsupported argumetns to array_load: "
          ++ show dst ++ " " ++ show arr ++ " "  ++ show index


    -- Section 7.10
    arrayAssignOp = do
      -- should be safe for the same reasons
      (VariableOperand arr) <- parseVariableOperand

      commaSep
      count <- parseIntOperand

      commaSep
      val <- parseConstOrVarOperand

      oprnds <- case (count, val) of 
        (VariableOperand v1, VariableOperand v2)
          -> return $ T.ArrAssignAVV (Shim.a2a arr) (Shim.v2v v1) (Shim.v2v v2)

        (ConstantOperand c IntType, VariableOperand v)
          -> return $ T.ArrAssignAIV (Shim.a2a arr) (Shim.c2i c) (Shim.v2v v)

        (ConstantOperand c1 IntType, ConstantOperand c2 IntType)
          -> return $ T.ArrAssignAII (Shim.a2a arr) (Shim.c2i c1) (Shim.c2i c2)

        (VariableOperand v, ConstantOperand c IntType)
          -> return $ T.ArrAssignAVI (Shim.a2a arr) (Shim.v2v v) (Shim.c2i c)

        _ -> error $
          "invalid args to array_store"
          ++ show arr ++ " " ++ show count ++ " " ++ show val
      
      return $ T.AssignArr oprnds

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

parseFunctionOperand :: Parsec' T.FunctionName
parseFunctionOperand = do
  T.FunctionName . T.Label <$> parseIdentifier

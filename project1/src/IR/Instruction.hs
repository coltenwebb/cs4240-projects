module IR.Instruction where

import IR.Type
import Data.Maybe
import Data.Data (ConstrRep(FloatConstr))
import Data.List
import qualified Data.Map as M

data Instruction = Instruction
  { opcode :: OpCode
  , operands :: [Operand]
  , lineNum :: LineNumber
  }
instance Eq Instruction where
  (Instruction _ _ (LineNumber n1)) == (Instruction _ _ (LineNumber n2)) = n1 == n2
instance Ord Instruction where
  compare (Instruction _ _ (LineNumber n1)) (Instruction _ _ (LineNumber n2)) = n1 `compare` n2
instance Show Instruction where
  show (Instruction op oprnds ln) =
    show ln ++ " [" ++ show op ++ "]"
    ++ concatMap (\x -> ", " ++ show x) oprnds

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq)

instance Show LineNumber where
  show (LineNumber ln) = "(L" ++ show ln ++ ")"

data OpCode =
  ASSIGN |
  ADD | SUB | MULT | DIV | AND | OR |
  GOTO |
  BREQ | BRNEQ | BRLT | BRGT | BRLEQ | BRGEQ |
  RETURN |
  CALLR | CALL |
  ARRAY_STORE | ARRAY_LOAD |
  LABEL
  deriving (Enum, Bounded, Eq)

instance Show OpCode where
  show opcode = case opcode of
    ASSIGN -> "assign"
    ADD -> "add"
    SUB -> "sub"
    MULT -> "mult"
    DIV -> "div"
    AND -> "and"
    OR -> "or"
    GOTO -> "goto"
    BREQ -> "breq"
    BRNEQ -> "brneq"
    BRLT -> "brlt"
    BRGT -> "brgt"
    BRLEQ -> "brleq"
    BRGEQ -> "brgeq"
    RETURN -> "return"
    CALL -> "call"
    CALLR -> "callr"
    ARRAY_STORE -> "array_store"
    ARRAY_LOAD -> "array_load"
    LABEL -> "label"

newtype FunctionName = FunctionName String deriving (Eq, Ord, Show)
newtype LabelName = LabelName String deriving (Eq, Ord, Show)
newtype VariableName = VariableName String deriving (Eq, Ord, Show)
newtype ConstantValue = ConstantValue String deriving (Eq, Ord, Show)

data Variable = Variable
  { variableName :: VariableName
  , variableType :: Type
  } deriving (Eq, Ord, Show)

data Operand = LabelOperand LabelName
  | FunctionOperand FunctionName
  | VariableOperand Variable
  | ConstantOperand ConstantValue Type
  deriving Eq -- TODO: prevent invalid types

instance Show Operand where
  show oprnd = case oprnd of
    LabelOperand (LabelName s) -> "(label: " ++ s ++ ")"
    FunctionOperand (FunctionName s) -> "(fn: " ++ s ++ ")"
    VariableOperand (Variable (VariableName s) vt) ->
      "(var " ++ show vt ++ ": " ++ s ++ ")"
    ConstantOperand (ConstantValue s) tp ->
      "(const " ++ show tp ++ ": " ++ s ++ ")"


arrayIndexOutofBound :: Operand -> Integer -> Bool
arrayIndexOutofBound operand i =
  case operand of
    VariableOperand (Variable _ (ArrayType (ArraySize size) _)) -> i >= size && i < 0
    _ -> False

isVariableOperand :: Operand -> Bool
isVariableOperand operand =
  case operand of
    VariableOperand _ -> True
    _ -> False

isConstantOperand :: Operand -> Bool
isConstantOperand operand = case operand of
  ConstantOperand _ _ -> True
  _ -> False

getType :: Operand -> Type
getType op = case op of
  VariableOperand (Variable _ tp) -> tp
  ConstantOperand _ tp -> tp
  _ -> VoidType


basicTypeMatches :: Operand -> Operand -> Bool
basicTypeMatches o1 o2 =
  (getType o1 == getType o2) && (isBasicType . getType) o1

arrayElementTypeMatches :: Operand -> Operand -> Bool
arrayElementTypeMatches arr o2 =
  (isArrayType . getType) arr && (isBasicType . getType) o2
    && (getType o2 == arrBasicType)
  where
    arrBasicType = (geElementType . getType) arr

    geElementType :: Type -> Type
    geElementType tp = case tp of
      ArrayType _ et -> et
      _ -> tp

operandIsBasicType :: Operand -> Bool
operandIsBasicType = isBasicType . getType

operandIsArrayType :: Operand -> Bool
operandIsArrayType = isArrayType . getType

operandIsIntBasicType :: Operand -> Bool
operandIsIntBasicType = (== IntType) . getType

isBasicType :: Type -> Bool
isBasicType tp = case tp of
  IntType -> True
  FloatType -> True
  _ -> False

isArrayType :: Type -> Bool
isArrayType tp = case tp of
  ArrayType _ _ -> True
  _ -> False


isVarOp (VariableOperand var) = Just var
isVarOp _ = Nothing

usedVars :: Instruction -> [Variable]
usedVars inst
  -- any variable not in the first operand is used
  | opcode inst `elem` [
      ASSIGN, ADD, SUB, MULT, DIV, AND, OR,
      BREQ, BRNEQ, BRLT, BRGT, BRGEQ, BRLEQ, CALLR, CALL, ARRAY_LOAD
    ] = mapMaybe isVarOp (drop 1 (operands inst ))
  -- only the variable in the first operand is used (if it is a variable)
  | opcode inst `elem` [
      RETURN
    ] = mapMaybe isVarOp (take 1 (operands inst ))
  | opcode inst `elem` [
      ARRAY_STORE
    ] = mapMaybe isVarOp (take 1 (operands inst)++drop 2 (operands inst))
  | otherwise = []

defVars :: Instruction -> Maybe Variable
defVars inst =
  case dv of
    [ ]  -> Nothing
    [x] -> Just x
    _ -> error ("More than one def vars in an instr. unpossible nani?!?! " ++ show inst)
  where
    dv = mapMaybe isVarOp (operands inst) \\ usedVars inst

defOpcodes :: [OpCode]
defOpcodes = [ASSIGN, ADD, SUB, MULT, DIV, AND, OR, CALLR, ARRAY_LOAD]

isDefOpcode :: OpCode -> Bool
isDefOpcode = (`elem` defOpcodes)

branchOpcodes :: [OpCode]
branchOpcodes = [BREQ, BRGEQ, BRGT, BRLEQ, BRLT, BRNEQ]

lineNumberMap :: [Instruction] -> M.Map LineNumber Instruction
lineNumberMap insts = M.fromList (map (\i -> (lineNum i, i)) insts)

isBranching :: Instruction -> Bool
isBranching inst
  | opcode inst `elem` [
      BREQ, BRNEQ, BRLT, BRGT, BRGEQ, BRLEQ, GOTO, RETURN
    ] = True
  | otherwise = False

isLabel :: Instruction -> Bool
isLabel = (== LABEL) . opcode

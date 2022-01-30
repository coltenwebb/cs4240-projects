module IR.Instruction where

import IR.Type
import Data.Maybe
import Data.Data (ConstrRep(FloatConstr))

newtype LineNumber = LineNumber Int

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

data Instruction = Instruction
  { opcode :: OpCode
  , operands :: [Operand]
  , lineNum :: LineNumber
  }

instance Show Instruction where
  show (Instruction op oprnds ln) =
    show ln ++ " [" ++ show op ++ "]"
    ++ concatMap (\x -> ", " ++ show x) oprnds
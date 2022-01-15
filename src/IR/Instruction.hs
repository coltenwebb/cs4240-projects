module IR.Instruction where

import IR.Type

newtype LineNumber = LineNumber Int
data OpCode =
  ASSIGN |
  ADD | SUB | MULT | DIV | AND | OR |
  GOTO |
  BREQ | BRNEQ | BRLT | BRGT | BRLEQ | BRGEQ |
  RETURN |
  CALL | CALLR |
  ARRAY_STORE | ARRAY_LOAD |
  LABEL

data OperandInfo = OperandInfo { name :: String, parent :: Instruction }

-- TODO: Check if necessary.
-- Annoying unfortunately, due to structure from starter code
data VarOperand = VarOperand OperandInfo Type

data Operand = LabelOperand OperandInfo
  | FunctionOperand OperandInfo
  | VariableOperand VarOperand
  | ConstantOperand OperandInfo Type

data Instruction = Instruction
  { opcode :: OpCode
  , operands :: [Operand]
  , lineNum :: LineNumber
  }
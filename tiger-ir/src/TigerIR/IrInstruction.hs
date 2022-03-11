{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module TigerIR.IrInstruction where

import TigerIR.Types

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq)

data IrInstruction = IrInstruction
  { instruction :: Instruction
  , lineNum     :: LineNumber
  }

data Instruction
  = AssignVar         AssignVarOperands
  | BinaryOperation   BinOp
  | BranchOperation   BrOp
  | Return            RetvarOperand
  | Call              FunctionName FnArgs
  | Callr             Variable FunctionName FnArgs
  | Goto              Label
  | ArrStore          ArrStoreOperands
  | ArrLoad           ArrLoadOperands
  | AssignArr         ArrAssignOperands


data AssignVarOperands
  = AssignVarOpsVV Variable Variable
  | AssignVarOpsVI Variable Imm

data BinOp
  = Add  BinOperands
  | Sub  BinOperands
  | Mult BinOperands
  | Div  BinOperands
  | And  BinOperands
  | Or   BinOperands

data BrOp
  = Breq  BrOperands
  | Brneq BrOperands
  | Brlt  BrOperands
  | Brgt  BrOperands
  | Brgeq BrOperands
  | Brleq BrOperands

data BinOperands
  = BinOpsVVV Variable Variable Variable
  | BinOpsVIV Variable Imm      Variable
  | BinOpsVII Variable Imm      Imm
  | BinOpsVVI Variable Variable Imm

data BrOperands
  = BrOpsVV Label Variable Variable
  | BrOpsVI Label Variable Imm
  | BrOpsIV Label Imm      Variable
  | BrOpsII Label Imm      Imm

-- Best we can do is manually make sure that a and b are consistent
data ArrStoreOperands
  -- array_store, a, arr, 0
  -- arr[0] := a
  = ArrStoreVAI Variable Array Imm
  | ArrStoreVAV Variable Array Variable

data ArrLoadOperands
  -- array_load, a, arr, 0
  -- a := arr[0]
  = ArrLoadVAI Variable Array Imm
  | ArrLoadVAV Variable Array Variable

data ArrAssignOperands
  -- assign, X, 100, 10
  -- type ArrayInt = array [100] of int;
  -- var X : ArrayInt := 10
  = ArrAssignII Array Imm Imm
  | ArrAssignIV Array Imm Variable
  | ArrAssignVI Array Variable Imm
  | ArrAssignVV Array Variable Variable

data RetvarOperand
  = Retvar Variable
  | Retimm Imm

--------------------------------------------------------------
-- LEGACY: Needed for parser to work
--------------------------------------------------------------
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
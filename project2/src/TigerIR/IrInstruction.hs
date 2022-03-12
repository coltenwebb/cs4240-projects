module TigerIR.IrInstruction where

import TigerIR.Types

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq, Show)

data Instruction a = Instruction
  { instruction :: a
  , lineNum     :: LineNumber
  }
  deriving (Show)
type TigerIrIns = Instruction IrInstruction 
type DestVar = Variable

data IrInstruction
  = AssignVar         AssignVarOperands
  | BinaryOperation   BinOp BinOperands
  | BranchOperation   BrOp BrOperands
  | Return            RetvarOperand
  | Call              FunctionName FnArgs
  | Callr             DestVar FunctionName FnArgs
  | Goto              Label
  | ArrStore          ArrStoreOperands
  | ArrLoad           ArrLoadOperands
  | AssignArr         ArrAssignOperands
  | LabelIns          Label
  deriving (Show)

data AssignVarOperands
  = AssignVarOpsDV DestVar Variable
  | AssignVarOpsDI DestVar Imm
  deriving (Show)

data BinOp
  = Add
  | Sub 
  | Mult
  | Div
  | And 
  | Or
  deriving (Show)

data BrOp
  = Breq
  | Brneq
  | Brlt
  | Brgt 
  | Brgeq
  | Brleq
  deriving (Show)

data BinOperands
  = BinOpsDVV DestVar Variable Variable
  | BinOpsDIV DestVar Imm      Variable
  | BinOpsDII DestVar Imm      Imm
  | BinOpsDVI DestVar Variable Imm
  deriving (Show)

data BrOperands
  = BrOpsVV Label Variable Variable
  | BrOpsVI Label Variable Imm
  | BrOpsIV Label Imm      Variable
  | BrOpsII Label Imm      Imm
  deriving (Show)

data ArrStoreOperands
  -- array_store, a, arr, 0
  -- arr[0] := a
  = ArrStoreVAI Variable Array Imm
  | ArrStoreVAV Variable Array Variable
  | ArrStoreIAI Imm      Array Imm
  | ArrStoreIAV Imm      Array Variable
  deriving (Show)

data ArrLoadOperands
  -- array_load, a, arr, 0
  -- a := arr[0]
  = ArrLoadDAI DestVar Array Imm
  | ArrLoadDAV DestVar Array Variable
  deriving (Show)

data ArrAssignOperands
  -- Array X implicit
  -- assign, X, 100, 10
  -- type ArrayInt = array [100] of int;
  -- var X : ArrayInt := 10
  = ArrAssignAII Array Imm Imm
  | ArrAssignAIV Array Imm Variable
  | ArrAssignAVI Array Variable Imm
  | ArrAssignAVV Array Variable Variable
  deriving (Show)

data RetvarOperand
  = Retvar Variable
  | Retimm Imm
  deriving (Show)

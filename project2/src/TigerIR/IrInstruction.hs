module TigerIR.IrInstruction where

import TigerIR.Types

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq, Show)

type TigerIrIns = Instruction IrInstruction
type DestVar = Variable

data Instruction a = Instruction
  { instruction :: a
  , lineNum     :: LineNumber
  }

fmtIr :: [String] -> String
fmtIr strs = head strs ++ " " ++ concatMap (++ " ") (tail strs)

spaceBtwn :: [String] -> String
spaceBtwn strs = init $ concatMap (++ " ") strs

data IrInstruction
  = AssignVar         AssignVarOperands
  | BinaryOperation   BinOp BinOperands
  | BranchOperation   BrOp Label BrOperands
  | Return            RetvarOperand
  | BeginFunction
  | EndFunction
  | Call              FunctionName FnArgs
  | Callr             DestVar FunctionName FnArgs
  | Goto              Label
  | ArrStore          ArrStoreOperands
  | ArrLoad           ArrLoadOperands
  | AssignArr         ArrAssignOperands
  | LabelIns          Label

instance Show IrInstruction where
  show inst = case inst of
    AssignVar avo             -> fmtIr ["    assign", show avo]
    BinaryOperation bo bo'    -> fmtIr ["   ", show bo, show bo']
    BranchOperation bo lb bo' -> fmtIr ["   ", show bo, show lb, show bo']
    Return ro                 -> fmtIr ["    return", show ro]
    Call fn fas               -> fmtIr ["    call", show fn, if null fas then "" else concatMap (\a -> show a ++ " ") fas]
    Callr vari fn fas         -> fmtIr ["    callr", show vari, show fn, if null fas then "" else concatMap (\a -> show a ++ " ") fas]
    Goto la                   -> fmtIr ["    goto", show la]
    ArrStore aso              -> fmtIr ["    store", show aso]
    ArrLoad alo               -> fmtIr ["    load", show alo]
    AssignArr aao             -> fmtIr ["    assign", show aao]
    LabelIns la               -> show la ++ ":"
    BeginFunction             -> ""
    EndFunction               -> ""

data AssignVarOperands
  = AssignVarOpsDV DestVar Variable
  | AssignVarOpsDI DestVar Imm
instance Show AssignVarOperands where
  show (AssignVarOpsDV dst var) = spaceBtwn [show dst, show var]
  show (AssignVarOpsDI dst imm) = spaceBtwn [show dst, show imm]


data BinOp
  = Add
  | Sub
  | Mult
  | Div
  | And
  | Or
instance Show BinOp where
  show op = case op of
    Add   -> "add"
    Sub   -> "sub"
    Mult  -> "mult"
    Div   -> "div"
    And   -> "and"
    Or    -> "or"

data BrOp
  = Breq
  | Brneq
  | Brlt
  | Brgt
  | Brgeq
  | Brleq
instance Show BrOp where
  show op = case op of
    Breq  -> "breq"
    Brneq -> "brneq"
    Brlt  -> "brlt"
    Brgt  -> "brgt"
    Brgeq -> "brgeq"
    Brleq -> "brleq"

data BinOperands
  = BinOpsDVV DestVar Variable Variable
  | BinOpsDIV DestVar Imm      Variable
  | BinOpsDII DestVar Imm      Imm
  | BinOpsDVI DestVar Variable Imm
instance Show BinOperands where
  show binops = case binops of
    BinOpsDVV vari vari' vari2  -> spaceBtwn [show vari, show vari', show vari2]
    BinOpsDIV vari imm vari'    -> spaceBtwn [show vari, show imm, show vari']
    BinOpsDII vari imm imm'     -> spaceBtwn [show vari, show imm, show imm']
    BinOpsDVI vari vari' imm    -> spaceBtwn [show vari, show vari', show imm]



data BrOperands
  = BrOpsVV Variable Variable
  | BrOpsVI Variable Imm
  | BrOpsIV Imm      Variable
  | BrOpsII Imm      Imm
instance Show BrOperands where
  show brops = case brops of
    BrOpsVV vari vari' -> spaceBtwn [show vari, show vari']
    BrOpsVI vari imm   -> spaceBtwn [show vari, show imm]
    BrOpsIV imm vari   -> spaceBtwn [show imm, show vari]
    BrOpsII imm imm'   -> spaceBtwn [show imm, show imm']


data ArrStoreOperands
  -- array_store, a, arr, 0
  -- arr[0] := a
  = ArrStoreVAI Variable Array Imm
  | ArrStoreVAV Variable Array Variable
  | ArrStoreIAI Imm      Array Imm
  | ArrStoreIAV Imm      Array Variable
instance Show ArrStoreOperands where
  show asops = case asops of
    ArrStoreVAI vari ar imm   -> spaceBtwn [show vari, show ar, show imm]
    ArrStoreVAV vari ar vari' -> spaceBtwn [show vari, show ar, show vari']
    ArrStoreIAI imm ar imm'   -> spaceBtwn [show imm, show ar, show imm']
    ArrStoreIAV imm ar vari   -> spaceBtwn [show imm, show ar, show vari]


data ArrLoadOperands
  -- array_load, a, arr, 0
  -- a := arr[0]
  = ArrLoadDAI DestVar Array Imm
  | ArrLoadDAV DestVar Array Variable
instance Show ArrLoadOperands where
  show alops = case alops of
    ArrLoadDAI vari ar imm    -> spaceBtwn [show vari, show ar, show imm]
    ArrLoadDAV vari ar vari'  -> spaceBtwn [show vari, show ar, show vari']

data ArrAssignOperands
  -- Array X implicit
  -- assign, X, 100, 10
  -- type ArrayInt = array [100] of int;
  -- var X : ArrayInt := 10
  = ArrAssignAII Array Imm Imm
  | ArrAssignAIV Array Imm Variable
instance Show ArrAssignOperands where
  show aaops = case aaops of
    ArrAssignAII ar imm imm'    -> spaceBtwn [show ar, show imm, show imm']
    ArrAssignAIV ar imm vari    -> spaceBtwn [show ar, show imm, show vari]

data RetvarOperand
  = Retvar Variable
  | Retimm Imm
instance Show RetvarOperand where
  show rvops = case rvops of
    Retvar vari -> show vari
    Retimm imm -> show imm

instance (Show a) => Show (Instruction a) where
  show (Instruction i _) = show i

-- commas :: [String] -> String
-- commas strs = init $ concatMap (++ ", ") strs
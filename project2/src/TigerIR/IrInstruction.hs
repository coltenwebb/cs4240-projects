module TigerIR.IrInstruction where

import TigerIR.Types

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq, Show)

type TigerIrIns = Instruction IrInstruction
type DestVar = Variable

fmtIr :: [String] -> String 
fmtIr strs = head strs ++ concatMap (++ " ") (tail strs)

spaceBtwn :: [String] -> String 
spaceBtwn strs = init $ concatMap (++ " ") strs

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
instance Show IrInstruction where
  show inst = case inst of 
    AssignVar avo           -> fmtIr ["assign", show avo]
    BinaryOperation bo bo'  -> fmtIr [show bo, show bo']
    BranchOperation bo bo'  -> fmtIr [show bo, show bo']
    Return ro               -> fmtIr ["return", show ro]
    Call fn fas             -> fmtIr ["call", show fn, show fas]
    Callr vari fn fas       -> fmtIr ["callr", show vari, show fn, show fas]
    Goto la                 -> fmtIr ["goto", show la]
    ArrStore aso            -> fmtIr ["store", show aso]
    ArrLoad alo             -> fmtIr ["load", show alo]
    AssignArr aao           -> fmtIr ["assign", show aao]
    LabelIns la             -> fmtIr ["label", show la]



data AssignVarOperands
  = AssignVarOpsDV DestVar Variable
  | AssignVarOpsDI DestVar Imm
instance Show AssignVarOperands where 
  show (AssignVarOpsDV dst var) = commas [show dst, show var] ++ "\n"
  show (AssignVarOpsDI dst imm) = commas [show dst, show imm] ++ "\n"


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
  = BrOpsVV Label Variable Variable
  | BrOpsVI Label Variable Imm
  | BrOpsIV Label Imm      Variable
  | BrOpsII Label Imm      Imm
instance Show BrOperands where 
  show brops = case brops of 
    BrOpsVV la vari vari' -> spaceBtwn [show la, show vari, show vari']
    BrOpsVI la vari imm   -> spaceBtwn [show la, show vari, show imm]
    BrOpsIV la imm vari   -> spaceBtwn [show la, show imm, show vari]
    BrOpsII la imm imm'   -> spaceBtwn [show la, show imm, show imm']

  

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
  | ArrAssignAVI Array Variable Imm
  | ArrAssignAVV Array Variable Variable
instance Show ArrAssignOperands where 
  show aaops = case aaops of 
    ArrAssignAII ar imm imm'    -> spaceBtwn [show ar, show imm, show imm']
    ArrAssignAIV ar imm vari    -> spaceBtwn [show ar, show imm, show vari]
    ArrAssignAVI ar vari imm    -> spaceBtwn [show ar, show vari, show imm]
    ArrAssignAVV ar vari vari'  -> spaceBtwn [show ar, show vari, show vari']

  

data RetvarOperand
  = Retvar Variable
  | Retimm Imm
instance Show RetvarOperand where 
  show rvops = case rvops of 
    Retvar vari -> show vari
    Retimm imm -> show imm

  

data Instruction a = Instruction
  { instruction :: a
  , lineNum     :: LineNumber
  }

instance (Show a) => Show (Instruction a) where 
  show (Instruction i (LineNumber n)) = show n ++ ":    " ++ show i

commas :: (Show a) => [a] -> String
commas strs = init $ concatMap (\str -> show str ++ ", ") strs
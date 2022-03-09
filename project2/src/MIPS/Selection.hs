module MIPS.Selection where

import TigerIR.IrInstruction as T
import MIPS.Types.Virtual as V
import MIPS.Types.Operand

import Data.Bits
import TigerIR.IrInstruction (BrOperands)

instructionSelection :: T.Instruction -> MipsVirtual
instructionSelection ins = case ins of
  BinaryOperation op -> case op of
    T.Add  bops -> handleBinOp bops addImm  V.Addi   V.Add
    T.Sub  bops -> handleBinOp bops subImm  V.Subi   V.Sub
    T.Mult bops -> handleBinOp bops multImm V.Multi  V.Mult
    T.Div  bops -> handleBinOp bops divImm  V.Divi   V.Div
    T.And  bops -> handleBinOp bops andImm  V.Andi   V.And
    T.Or   bops -> handleBinOp bops orImm   V.Ori    V.Or

  BranchOperation op -> case op of
    T.Breq   brops -> handleBrOp brops Eq
    T.Brneq  brops -> handleBrOp brops Neq
    T.Brlt   brops -> handleBrOp brops Lt
    T.Brgt   brops -> handleBrOp brops Gt
    T.Brgeq  brops -> handleBrOp brops Geq
    T.Brleq  brops -> handleBrOp brops Leq

  T.Return retvarOp -> case retvarOp of
    Retvar v -> V.Return (VReg v)
    Retimm i -> V.Returni i
  
  T.Call (FunctionName lab) params
    -> V.Call lab (fnArgsToCallArgs params)

  T.Callr v (FunctionName lab) params
    -> V.Callr (VReg v) lab (fnArgsToCallArgs params)
  
  T.Goto lab -> V.Goto lab

  T.ArrStore arrStrOps -> case arrStrOps of
    ArrStoreVAV v1 (Array arr _) v2
      -> V.ArrStr (VReg v1) (VReg arr) (VReg v2)

    ArrStoreVAI v (Array arr _) i
      -> V.ArrStri (VReg v) (VReg arr) i
  
  T.ArrLoad arrLdOps -> case arrLdOps of
    ArrLoadVAV v1 (Array arr _) v2
      -> V.ArrLoad (VReg v1) (VReg arr) (VReg v2)

    ArrLoadVAI v (Array arr _) i
      -> V.ArrLoadi (VReg v) (VReg arr) i

  T.AssignArr asses -> case asses of
    T.ArrAssignII (Array arr _) i1 i2
      -> V.ArrAssignII (VReg arr) i1 i2
    
    T.ArrAssignIV (Array arr _) i v
      -> V.ArrAssignIV (VReg arr) i (VReg v)
    
    T.ArrAssignVI (Array arr _) v i
      -> V.ArrAssignVI (VReg arr) (VReg v) i
    
    T.ArrAssignVV (Array arr _) v1 v2
      -> V.ArrAssignVV (VReg arr) (VReg v1) (VReg v2)

handleBinOp
  :: BinOperands
  -> (Imm -> Imm -> Imm)
  -> (VReg -> VReg -> Imm -> MipsVirtual)
  -> (VReg -> VReg -> VReg -> MipsVirtual)
  -> MipsVirtual
handleBinOp bops immHandler immIns regIns =
  case bops of
    BinOpsVII v1 i2 i3 -> V.Li (VReg v1) (immHandler i2 i3)
    BinOpsVIV v1 i2 v3 -> immIns (VReg v1) (VReg v3) i2
    BinOpsVVI v1 v2 i3 -> immIns (VReg v1) (VReg v2) i3
    BinOpsVVV v1 v2 v3 -> regIns (VReg v1) (VReg v2) (VReg v3)

liftImm :: (Int -> Int -> Int) -> Imm -> Imm -> Imm
liftImm f (Imm i1) (Imm i2) = Imm (show i3')
  where
    i1' = read i1 :: Int
    i2' = read i2 :: Int
    i3' = f i1' i2'

addImm, subImm, multImm, divImm, andImm, orImm  :: Imm -> Imm -> Imm
addImm  = liftImm (+)
subImm  = liftImm (-)
multImm = liftImm (*)
divImm  = liftImm div
andImm  = liftImm (.&.)
orImm   = liftImm (.|.)

handleBrOp
  :: BrOperands
  -> Cmp
  -> MipsVirtual
handleBrOp brops cmp =
  case brops of
    BrOpsVV lab v1 v2 -> V.Br  cmp (VReg v1) (VReg v2) lab
    BrOpsVI lab v1 i2 -> V.Bri cmp (VReg v1) i2 lab
    BrOpsIV lab i1 v2 -> V.Bri cmp (VReg v2) i1 lab
    BrOpsII lab i1 i2 ->
      if i1 == i2
        then V.Goto lab
        else V.Nop

fnArgsToCallArgs :: FnArgs -> [CallArg]
fnArgsToCallArgs (FnArgs ags) = map ps ags
  where
    ps :: FnArg -> CallArg
    ps farg = case farg of
      Varg v           -> CVarg (VReg v)
      Aarg (Array v _) -> CVarg (VReg v)
      Iarg i           -> CIarg i


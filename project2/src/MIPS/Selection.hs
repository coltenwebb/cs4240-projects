{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
module MIPS.Selection where

import TigerIR.IrInstruction as T
import TigerIR.Program as T
import TigerIR.Types as T
import MIPS.Types.Virtual as V
import MIPS.Types.Physical as P
import MIPS.RegisterAllocator.Naive
import MIPS.CallingConvention
import Data.Bits
import Data.List
import MIPS.Types.Operand

-- note: (. instruction) record selector, discard the line number
virtFnSelection :: TigerIrFunction -> V.VirtualFunction
virtFnSelection = instrSelectionPass (instructionSelection . instruction)

--functionSelection :: T.TigerIrFunction -> [P.MipsPhys]
--functionSelection fn = foldl' v2p [] vinsts
--  where
--    v2p :: [P.MipsPhys] -> V.MipsVirtual -> [P.MipsPhys]
--    v2p acc vinst = acc ++ virtToPhysMIPS regMap vinst 
--
--    vinsts :: [V.MipsVirtual]
--    vinsts = V.Label (T.Label (fnameStr fn)) : map (instructionSelection . instruction) (T.instrs fn)
--
--    regMap :: RegMap
--    regMap = genRegMap vregs 
--      where 
--        vregs = map toVReg (parameters fn) ++ map toVReg (localVars fn)
--
--    fnameStr :: T.TigerIrFunction -> String
--    fnameStr (T.TigerIrFunction (T.FunctionName (T.Label fname)) _ _ _ _) = fname

instructionSelection :: T.IrInstruction -> MipsVirtual
instructionSelection ins = case ins of
  AssignVar avops -> case avops of
    AssignVarOpsDI v1 i2 -> V.AssignI (VReg v1) i2
    AssignVarOpsDV v1 v2 -> V.AssignV (VReg v1) (VReg v2)

  BinaryOperation op bops -> case op of
    T.Add  -> handleCommutativeBinOp bops addImm  V.Addi   V.Add
    T.Mult -> handleCommutativeBinOp bops multImm V.Multi  V.Mult
    T.And  -> handleCommutativeBinOp bops andImm  V.Andi   V.And
    T.Or   -> handleCommutativeBinOp bops orImm   V.Ori    V.Or
    
    T.Sub  -> handleNonCommutativeBinOp bops subImm V.SubVI V.SubIV V.Sub
    T.Div  -> handleNonCommutativeBinOp bops divImm V.DivVI V.DivIV V.Div

  BranchOperation op brops -> case op of
    T.Breq  -> handleBrOp brops Eq
    T.Brneq -> handleBrOp brops Neq
    T.Brlt  -> handleBrOp brops Lt
    T.Brgt  -> handleBrOp brops Gt
    T.Brgeq -> handleBrOp brops Geq
    T.Brleq -> handleBrOp brops Leq

  T.Return retvarOp -> case retvarOp of
    Retvar v -> V.Return (VReg v)
    Retimm i -> V.Returni i

  T.BeginFunction -> V.BeginFunction
  
  T.EndFunction -> V.EndFunction

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

    ArrStoreIAI val (Array arr _) idx
      -> V.ArrStrii val (VReg arr) idx

    ArrStoreIAV val (Array arr _) idx
      -> V.ArrStriv val (VReg arr) (VReg idx)

  T.ArrLoad arrLdOps -> case arrLdOps of
    ArrLoadDAV v1 (Array arr _) v2
      -> V.ArrLoad (VReg v1) (VReg arr) (VReg v2)

    ArrLoadDAI v (Array arr _) i
      -> V.ArrLoadi (VReg v) (VReg arr) i

  T.AssignArr asses -> case asses of
    T.ArrAssignAII (Array arr _) i1 i2
      -> V.ArrAssignII (VReg arr) i1 i2

    T.ArrAssignAIV (Array arr _) i v
      -> V.ArrAssignIV (VReg arr) i (VReg v)

    T.ArrAssignAVI (Array arr _) v i
      -> V.ArrAssignVI (VReg arr) (VReg v) i

    T.ArrAssignAVV (Array arr _) v1 v2
      -> V.ArrAssignVV (VReg arr) (VReg v1) (VReg v2)

  T.LabelIns label -> V.Label label


handleCommutativeBinOp
  :: BinOperands
  -> (Imm -> Imm -> Imm)
  -> (VReg -> VReg -> Imm -> MipsVirtual)
  -> (VReg -> VReg -> VReg -> MipsVirtual)
  -> MipsVirtual
handleCommutativeBinOp bops immHandler immIns regIns =
  case bops of
    BinOpsDII v1 i2 i3 -> V.Li   (VReg v1) (immHandler i2 i3)
    BinOpsDIV v1 i2 v3 -> immIns (VReg v1) (VReg v3) i2
    BinOpsDVI v1 v2 i3 -> immIns (VReg v1) (VReg v2) i3
    BinOpsDVV v1 v2 v3 -> regIns (VReg v1) (VReg v2) (VReg v3)

handleNonCommutativeBinOp
  :: BinOperands
  -> (Imm -> Imm -> Imm)
  -> (VReg -> VReg -> Imm -> MipsVirtual)
  -> (VReg -> Imm  -> VReg -> MipsVirtual)
  -> (VReg -> VReg -> VReg -> MipsVirtual)
  -> MipsVirtual
handleNonCommutativeBinOp bops immHandler viIns ivIns regIns =
  case bops of
    BinOpsDII v1 i2 i3 -> V.Li   (VReg v1) (immHandler i2 i3)
    BinOpsDIV v1 i2 v3 -> ivIns  (VReg v1) i2 (VReg v3)
    BinOpsDVI v1 v2 i3 -> viIns  (VReg v1) (VReg v2) i3
    BinOpsDVV v1 v2 v3 -> regIns (VReg v1) (VReg v2) (VReg v3)

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
fnArgsToCallArgs = map ps
  where
    ps :: FnArg -> CallArg
    ps farg = case farg of
      Varg v           -> CVarg (VReg v)
      Aarg (Array v _) -> CVarg (VReg v)
      Iarg i           -> CIarg i



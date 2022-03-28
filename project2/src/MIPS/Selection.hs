{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
module MIPS.Selection where

import TigerIR.IrInstruction as T
import TigerIR.Program as T
import TigerIR.Types as T
import MIPS.Types.Virtual as V
import MIPS.CallingConvention
import Data.Bits
import Data.List
import MIPS.Types.Operand

-- note: (. instruction) record selector, discard the line number
virtFnSelection :: TigerIrFunction -> V.VirtualFunction
virtFnSelection = instrSelectionPass (instructionSelection . instruction)

instructionSelection :: T.IrInstruction -> MipsVirtual
instructionSelection ins = case ins of
  AssignVar avops -> case avops of
    AssignVarOpsDI v1 i2 -> V.Li (VReg v1) i2
    AssignVarOpsDV v1 v2 -> V.Assign (VReg v1) (VReg v2)

  BinaryOperation op bops -> case op of
    T.Add  -> handleCommutativeBinOp bops addImm  V.Addi   V.Add
    T.And  -> handleCommutativeBinOp bops andImm  V.Andi   V.And
    T.Or   -> handleCommutativeBinOp bops orImm   V.Ori    V.Or
    T.Mult -> handleCommutativeBinOp bops multImm V.Multi  V.Mult
    
    T.Sub  -> handleNonImmBinOp bops subImm  V.SubVI  V.SubIV  V.Sub
    T.Div  -> handleNonImmBinOp bops divImm  V.DivVI  V.DivIV  V.Div

  BranchOperation brop label oprnds -> case oprnds of
    BrOpsVV v1 v2 -> BrVV brop (VReg v1) (VReg v2) label
    BrOpsVI v1 i2 -> BrVI brop (VReg v1) i2 label
    BrOpsIV i1 v2 -> BrIV brop i1 (VReg v2) label
    BrOpsII i1 i2 -> BrII brop i1 i2 label

  T.Return retvarOp -> case retvarOp of
    Retvar v -> V.Return (VReg v)
    Retimm i -> V.Returni i

  T.BeginFunction -> V.BeginFunction
  
  T.EndFunction -> V.EndFunction

  T.Call (FunctionName lab) params -> V.Call lab (fnArgsToCallArgs params)

  T.Callr v (FunctionName lab) params ->
    V.Callr (VReg v) lab (fnArgsToCallArgs params)

  T.Goto lab -> V.Goto lab

  T.ArrStore arrStrOps -> case arrStrOps of
    ArrStoreVAV v1 (Array arr _) v2 ->
      V.ArrStrVAV (VReg v1) (VReg arr) (VReg v2)

    ArrStoreVAI v (Array arr _) i ->
      V.ArrStrVAI (VReg v) (VReg arr) i

    ArrStoreIAI val (Array arr _) idx ->
      V.ArrStrIAI val (VReg arr) idx

    ArrStoreIAV val (Array arr _) idx ->
      V.ArrStrIAV val (VReg arr) (VReg idx)

  T.ArrLoad arrLdOps -> case arrLdOps of
    ArrLoadDAV v1 (Array arr _) v2 ->
      V.ArrLoadV (VReg v1) (VReg arr) (VReg v2)

    ArrLoadDAI v (Array arr _) i ->
      V.ArrLoadI (VReg v) (VReg arr) i

  T.AssignArr asses -> case asses of
    T.ArrAssignAII (Array arr _) i1 i2 ->
      V.ArrAssignAII (VReg arr) i1 i2

    T.ArrAssignAIV (Array arr _) i v ->
      V.ArrAssignAIV (VReg arr) i (VReg v)
    
    T.ArrAssignAVI (Array arr _) v i ->
      V.ArrAssignAVI (VReg arr) (VReg v) i
    
    T.ArrAssignAVV (Array arr _) v1 v2 ->
      V.ArrAssignAVV (VReg arr) (VReg v1) (VReg v2)

  T.LabelIns label -> V.LabelIns label


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

handleNonImmBinOp
  :: BinOperands
  -> (Imm -> Imm -> Imm)
  -> (VReg -> VReg -> Imm  -> MipsVirtual)
  -> (VReg -> Imm  -> VReg -> MipsVirtual)
  -> (VReg -> VReg -> VReg -> MipsVirtual)
  -> MipsVirtual
handleNonImmBinOp bops immHandler vi iv vv = 
  case bops of
    BinOpsDII v1 i2 i3 -> V.Li (VReg v1) (immHandler i2 i3)
    BinOpsDIV v1 i2 v3 -> iv (VReg v1) i2 (VReg v3)
    BinOpsDVI v1 v2 i3 -> vi (VReg v1) (VReg v2) i3
    BinOpsDVV v1 v2 v3 -> vv (VReg v1) (VReg v2) (VReg v3)

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

fnArgsToCallArgs :: FnArgs -> [CallArg]
fnArgsToCallArgs = map ps
  where
    ps :: FnArg -> CallArg
    ps farg = case farg of
      Varg v           -> CVarg (VReg v)
      Aarg (Array v _) -> CVarg (VReg v)
      Iarg i           -> CIarg i

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

--  BranchOperation op label brops ->
--    let (a, b, loads) = case brops of
--                          BrOpsVV v1 v2 -> (VReg v1, VReg v2, [])
--                          BrOpsVI v1 i2 -> (VReg v1, toVReg i2, [ V.Li (toVReg i2) i2 ])
--                          BrOpsIV i1 v2 -> (toVReg i1, VReg v2, [ V.Li (toVReg i1) i1 ])
--                          -- can be simplified into goto at compile-time, but too lazy lmao
--                          BrOpsII i1 i2 -> (toVReg i1, toVReg i2,
--                            [ V.Li (toVReg i1) i1, V.Li (toVReg i2) i2])
--        -- a hack, but *-prefix should guarantee no vreg collision
--        branchValReg = VReg (Variable "*branch_val")
--    in loads ++ case op of
--      T.Breq  -> [ V.Beq a b label ]
--      T.Brneq -> [ V.Bne a b label ]
--      -- a < b <===> 0 < b - a
--      T.Brlt  -> [ V.Sub branchValReg b a, V.Bgtz branchValReg label ]
--
--      -- a > b <===> 0 < a - b
--      T.Brgt  -> [ V.Sub branchValReg a b, V.Bgtz branchValReg label ]
--
--      -- a >= b <===> b - a <= 0
--      T.Brgeq -> [ V.Sub branchValReg b a, V.Blez branchValReg label ]
--
--      -- a <= b <===> a - b <= 0
--      T.Brleq -> [ V.Sub branchValReg a b, V.Blez branchValReg label ]

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
      V.ArrStrVV (VReg v1) (VReg arr) (VReg v2)

    ArrStoreVAI v (Array arr _) i ->
      V.ArrStrVI (VReg v) (VReg arr) i

    ArrStoreIAI val (Array arr _) idx ->
      V.ArrStrII val (VReg arr) idx

    ArrStoreIAV val (Array arr _) idx ->
      V.ArrStrIV val (VReg arr) (VReg idx)

  T.ArrLoad arrLdOps -> case arrLdOps of
    ArrLoadDAV v1 (Array arr _) v2 ->
      V.ArrLoadV (VReg v1) (VReg arr) (VReg v2)

    ArrLoadDAI v (Array arr _) i ->
      V.ArrLoadI (VReg v) (VReg arr) i

  T.AssignArr asses -> case asses of
    T.ArrAssignAII (Array arr _) i1 i2 ->
      V.ArrAssignI (VReg arr) i1 i2

    T.ArrAssignAIV (Array arr _) i v ->
      V.ArrAssignV (VReg arr) i (VReg v)

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

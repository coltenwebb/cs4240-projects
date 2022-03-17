{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
module MIPS.RegisterAllocator.Naive where

import MIPS.Types.Operand
import MIPS.CallingConvention
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V

import qualified Data.Map as M
import Data.Foldable
import TigerIR.Program (instrSelectionPassFlatten, Function (parameters, localVars))

physFnSelection :: V.VirtualFunction -> P.PhysicalFunction
physFnSelection fn =
    instrSelectionPassFlatten (virtToPhysMIPS fn) fn
  where
    regMap :: RegMap
    regMap = calcRegMap fn

virtToPhysMIPS
  :: V.VirtualFunction
  -> V.MipsVirtual
  -> [P.MipsPhys]
virtToPhysMIPS vf mv = case mv of
  V.Addi t s i ->
    [ P.Lw (M M1) (k s) Fp
    , P.Addi (M M1) (M M1) i
    , P.Sw (M M1) (k t) Fp
    ]

  V.Add d s t ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M2) (k t) Fp
    , P.Add (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Fp
    ]

  V.Sub d s t ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M2) (k t) Fp
    , P.Sub (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Fp
    ]

  V.SubVI t s i ->
    [ P.Lw (M M1) (k s) Fp
    , P.Li (M M2) i
    , P.Sub (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k t) Fp
    ]
  
  V.SubIV t i s ->
    [ P.Li (M M1) i
    , P.Lw (M M2) (k s) Fp
    , P.Sub (M M1) (M M2) (M M2)
    , P.Sw (M M1) (k t) Fp
    ]

  V.Mult d s t ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M2) (k t) Fp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Fp
    ]

  V.Multi t s i ->
    [ P.Lw (M M1) (k s) Fp
    , P.Addi (M M2) ZeroReg i
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k t) Fp
    ]

  V.Div d s t ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M2) (k t) Fp
    , P.Div (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Fp
    ]
  
  V.DivVI t s i ->
    [ P.Lw (M M1) (k s) Fp
    , P.Li (M M2) i
    , P.Div (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k t) Fp
    ]

  V.DivIV t i s ->
    [ P.Li (M M1) i
    , P.Lw (M M2) (k s) Fp
    , P.Div (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k t) Fp
    ]

  V.Andi t s i ->
    [ P.Lw (M M1) (k s) Fp
    , P.Andi (M M1) (M M1) i
    , P.Sw (M M1) (k t) Fp
    ]

  V.And d s t ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M2) (k t) Fp
    , P.And (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Fp
    ]

  V.Ori t s i ->
    [ P.Lw (M M1) (k s) Fp
    , P.Ori (M M1) (M M1) i
    , P.Sw (M M1) (k t) Fp
    ]

  V.Or d s t ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M2) (k t) Fp
    , P.Or (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Fp
    ]

  V.Br c a b l -> case c of
    V.Eq -> [ P.Lw (M M1) (k a) Fp
          , P.Lw (M M2) (k b) Fp
          , P.Beq (M M1) (M M2) l]
    V.Neq -> [ P.Lw (M M1) (k a) Fp
           , P.Lw (M M2) (k b) Fp
           , P.Bne (M M1) (M M2) l]
    V.Lt -> [ P.Lw (M M1) (k a) Fp
           , P.Lw (M M2) (k b) Fp
           , P.Sub (M M1) (M M2) (M M1)
           , P.Bgtz (M M1) l]
    V.Gt -> [ P.Lw (M M1) (k a) Fp
           , P.Lw (M M2) (k b) Fp
           , P.Sub (M M1) (M M1) (M M2)
           , P.Bgtz (M M1) l]
    V.Geq -> [ P.Lw (M M1) (k a) Fp
           , P.Lw (M M2) (k b) Fp
           , P.Sub (M M1) (M M2) (M M1)
           , P.Blez (M M1) l]
    V.Leq -> [ P.Lw (M M1) (k a) Fp
           , P.Lw (M M2) (k b) Fp
           , P.Sub (M M1) (M M1) (M M2)
           , P.Blez (M M1) l]

  V.Bri c a i l -> case c of
    V.Eq -> [ P.Lw (M M1) (k a) Fp
          , P.Addi (M M2) ZeroReg i
          , P.Beq (M M1) (M M2) l]
    V.Neq -> [ P.Lw (M M1) (k a) Fp
           , P.Addi (M M2) ZeroReg i
           , P.Bne (M M1) (M M2) l]
    V.Lt -> [ P.Lw (M M1) (k a) Fp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M2) (M M1)
           , P.Bgtz (M M1) l]
    V.Gt -> [ P.Lw (M M1) (k a) Fp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M1) (M M2)
           , P.Bgtz (M M1) l]
    V.Geq -> [ P.Lw (M M1) (k a) Fp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M2) (M M1)
           , P.Blez (M M1) l]

    V.Leq -> [ P.Lw (M M1) (k a) Fp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M1) (M M2)
           , P.Blez (M M1) l]

  V.Lw t i s ->
    [ P.Lw (M M1) (k s) Fp
    , P.Lw (M M1) i (M M1)
    , P.Sw (M M1) (k t) Fp
    ]

  V.Li d i ->
    [ P.Addi (M M1) ZeroReg i
    , P.Sw (M M1) (k d) Fp ]

  V.Sw t i s ->
    [ P.Lw (M M1) (k s) Fp
    , P.Sw (M M2) i (M M1)
    , P.Sw (M M2) (k t) Fp
    ]

  V.Label lab -> [P.Label lab]

  V.Goto lab -> setupGoto lab

  V.Call fn args -> setupCallStack fn args loadReg

  V.Callr retReg fn args ->
   setupCallStack fn args loadReg
     ++ [ P.Sw Retval (k retReg) Fp]

  V.AssignI d i ->
    [ P.Addi (M M1) ZeroReg i
    , P.Sw (M M1) (k d) Fp]

  V.AssignV d s ->
    [ P.Lw (M M1) (k s) Fp
    , P.Sw (M M1) (k d) Fp ]

  V.ArrStr s a i ->
    [ P.Addi (M M1) ZeroReg imm4
    , P.Lw (M M2) (k i) Fp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Lw (M M2) (k a) Fp
    , P.Add (M M1) (M M1) (M M2)
    , P.Lw (M M2) (k s) Fp
    , P.Sw (M M2) (Imm "0") (M M1) ]

  V.ArrStriv s a i ->
    [ P.Addi (M M1) ZeroReg imm4
    , P.Lw (M M2) (k i) Fp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Lw (M M2) (k a) Fp
    , P.Add (M M1) (M M1) (M M2)
    , P.Addi (M M2) ZeroReg s
    , P.Sw (M M2) (Imm "0") (M M1) ]

  V.ArrStri s a i ->
    [ P.Lw (M M1) (k a) Fp
    , P.Lw (M M2) (k s) Fp
    , P.Sw (M M2) (times4 i) (M M1) ]

  V.ArrStrii s a i ->
     [ P.Lw (M M1) (k a) Fp
    , P.Addi (M M2) ZeroReg s
    , P.Sw (M M2) (times4 i) (M M1) ]

  V.ArrLoad d a i ->
    [ P.Addi (M M1) ZeroReg imm4
    , P.Lw (M M2) (k i) Fp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Lw (M M2) (k a) Fp
    , P.Add (M M1) (M M1) (M M2)
    , P.Lw (M M2) imm0 (M M1)
    , P.Sw (M M2) (k d) Fp ]

  V.ArrLoadi d a i ->
    [ P.Lw (M M1) (k a) Fp
    , P.Lw (M M2) (times4 i) (M M1)
    , P.Sw (M M2) (k d) Fp ]

  V.Nop -> []

  V.Return r -> setupReturn r loadReg

  V.Returni i -> setupReturnImm i

  -- TODO: pushing for colten first
  V.BeginFunction -> []

  V.EndFunction -> setupReturnVoid

  V.ArrAssignVV v1 v2 v3 -> setupCallStack (Label "memset")
    (map V.CVarg [v1, v2, v3]) loadReg

  V.ArrAssignVI v1 v2 i3 -> setupCallStack (Label "memset")
    [V.CVarg v1, V.CVarg v2, V.CIarg i3] loadReg

  V.ArrAssignII v1 i2 i3 -> setupCallStack (Label "memset")
    [V.CVarg v1, V.CIarg i2, V.CIarg i3] loadReg

  V.ArrAssignIV v1 i2 v3 -> setupCallStack (Label "memset")
    [V.CVarg v1, V.CIarg i2, V.CVarg v3] loadReg

  where
    rm :: RegMap
    rm = calcRegMap vf
    -- This is safe because genRegMap has assigned
    -- every virtual register its own unique index
    k :: VReg -> Imm
    k = toImm . (M.!) rm

    imm4 = Imm "4"
    imm0 = Imm "0"

    times4 :: Imm -> Imm
    times4 (Imm i) = Imm (show $ (read i :: Int) * 4)

    loadReg :: VReg -> (PReg, [P.MipsPhys])
    loadReg vreg =
      (M M1, [ P.Lw (M M1) (k vreg) Fp ])

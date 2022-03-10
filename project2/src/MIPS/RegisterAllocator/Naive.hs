module MIPS.RegisterAllocator.Naive where

import MIPS.Types.Operand
import MIPS.CallingConvention
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V

import qualified Data.Map as M
import Data.Foldable

genRegMap :: [VReg] -> RegMap
genRegMap = fst . foldl' f (mempty, 0)
  where
    f (c, idx) vr = (c <> M.singleton vr (OffsetIdx idx), idx+1)

virtToPhysMIPS
  :: RegMap
  -> V.MipsVirtual
  -> [P.MipsPhys]
virtToPhysMIPS rm mv = case mv of
  V.Addi t s i ->
    [ P.Lw (M M1) (k s) Sp
    , P.Addi (M M1) (M M1) i
    , P.Sw (M M1) (k t) Sp
    ]

  V.Add d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Add (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Sub d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Sub (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Subi t s i ->
    [ P.Lw (M M1) (k s) Sp
    , P.Addi (M M2) ZeroReg i
    , P.Sub (M M1) (M M1) (M M2)
    . P.Sw (M M1) (k t) Sp
    ]

  V.Mult d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Multi t s i ->
    [ P.Lw (M M1) (k s) Sp
    , P.Addi (M M2) ZeroReg i
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Div d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Div (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Divi t s i ->
    [ P.Lw (M M1) (k s) Sp
    , P.Addi (M M2) ZeroReg i
    , P.Div (M M1) (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Sp]

  V.Andi t s i ->
    [ P.Lw (M M1) (k s) Sp
    , P.Andi (M M1) (M M1) i
    , P.Sw (M M1) (k t) Sp
    ]

  V.And d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.And (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Ori t s i ->
    [ P.Lw (M M1) (k s) Sp
    , P.Ori (M M1) (M M1) i
    , P.Sw (M M1) (k t) Sp
    ]

  V.Or d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Or (M M1) (M M1) (M M2)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Br c a b l -> case c of
    Eq -> [ P.Lw (M M1) (k a) Sp
          , P.Lw (M M2) (k b) Sp
          , P.Beq (M M1) (M M2) l]
    Neq -> [ P.Lw (M M1) (k a) Sp
           , P.Lw (M M2) (k b) Sp
           , P.Bne (M M1) (M M2) l]
    Lt -> [ P.Lw (M M1) (k a) Sp
           , P.Lw (M M2) (k b) Sp
           , P.Sub (M M1) (M M2) (M M1)
           , P.Bgtz (M M1) l]
    Gt -> [ P.Lw (M M1) (k a) Sp
           , P.Lw (M M2) (k b) Sp
           , P.Sub (M M1) (M M1) (M M2)
           , P.Bgtz (M M1) l]
    Geq -> [ P.Lw (M M1) (k a) Sp
           , P.Lw (M M2) (k b) Sp
           , P.Sub (M M1) (M M2) (M M1)
           , P.Blez (M M1) l]
    Leq -> [ P.Lw (M M1) (k a) Sp
           , P.Lw (M M2) (k b) Sp
           , P.Sub (M M1) (M M1) (M M2)
           , P.Blez (M M1) l]

  V.Bri c a i l -> case c of
    Eq -> [ P.Lw (M M1) (k a) Sp
          , P.Addi (M M2) ZeroReg i
          , P.Beq (M M1) (M M2) l]
    Neq -> [ P.Lw (M M1) (k a) Sp
           , P.Addi (M M2) ZeroReg i
           , P.Bne (M M1) (M M2) l]
    Lt -> [ P.Lw (M M1) (k a) Sp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M2) (M M1)
           , P.Bgtz (M M1) l]
    Gt -> [ P.Lw (M M1) (k a) Sp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M1) (M M2)
           , P.Bgtz (M M1) l]
    Geq -> [ P.Lw (M M1) (k a) Sp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M2) (M M1)
           , P.Blez (M M1) l]
    Leq -> [ P.Lw (M M1) (k a) Sp
           , P.Addi (M M2) ZeroReg i
           , P.Sub (M M1) (M M1) (M M2)
           , P.Blez (M M1) l]

  V.Label l -> P.Label l

  V.Lw t i s ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M1) i (M M1)
    , P.Sw (M M1) (k t) Sp
    ]

  V.Li d i ->
    [ P.Addi (M M1) ZeroReg i
    , P.Sw (M M1) (k d) Sp ]

  V.Sw t i s ->
    [ P.Lw (M M1) (k s) Sp
    , P.Sw (M M2) i (M M1)
    , P.Sw (M M2) (k t) Sp
    ]

  V.Label lab -> [P.Label lab]

  V.Goto lab -> setupGoto lab

  V.Call fn args -> setupCallStack fn args loadReg

  V.Callr retReg fn args ->
   setupCallStack fn args loadReg
     ++ [ P.Sw Retval (k retReg) Sp]

  V.AssignI d i ->
    [ P.Addi (M M1) ZeroReg i
    , P.Sw (M M1) (k d) Sp]

  V.AssignV d s ->
    [ P.Lw (M M1) (k s) Sp
    , P.Sw (M M1) (k d) Sp ]

  V.ArrStr s a i ->
    [ P.Addi (M M1) ZeroReg imm4
    , P.Lw (M M2) (k i) Sp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Lw (M M2) (k a) Sp
    , P.Add (M M1) (M M1) (M M2)
    , P.Lw (M M2) (k s) Sp
    , P.Sw (M M2) (Imm "0") M1 ]

  V.ArrStri s a i ->
    [ P.Lw (M M1) (k a) Sp
    , P.Lw (M M2) (k s) Sp
    , P.Sw (M M2) (times4 i) (M M1) ]

  V.ArrLoad d a i ->
    [ P.Addi (M M1) ZeroReg imm4
    , P.Lw (M M2) (k i) Sp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Lw (M M2) (k a) Sp
    , P.Add (M M1) (M M1) (M M2)
    , P.Lw (M M2) imm0 M1
    . P.Sw (M M2) (k d) Sp ]

  V.ArrLoadi d a i ->
    [ P.Lw (M M1) (k a) Sp
    , P.Lw (M M2) (times4 i) (M M1)
    , P.Sw (M M2) (k d) Sp ]

  -- V.ArrAssignIV x (Imm s) v ->
  --   [ P.Lw (M M1) (k v) Sp
  --   , P.Lw (M M2) (k x) Sp ]
  --   ++ arrAssignI_ $ (read s :: Int) - 1

  -- V.ArrAssignII x (Imm s) v ->
  --   [ P.Addi (M M1) ZeroReg v
  --   , P.Lw (M M2) (k x) Sp ]
  --   ++ arrAssignI_ $ (read s :: Int) - 1

  V.ArrAssignVV x s v -> memset (x,s,v)
  V.ArrAssignVI x s v -> memset (x,s,v)
  V.ArrAssignII x s v -> memset (x,s,v)
  V.ArrAssignIV x s v -> memset (x,s,v)

  V.Nop -> []

  -- TODO: Should we pass in netLocalVarSize as a param?
  V.Return r -> setupReturn (Just r) loadReg 

  V.Returni i -> [ P.Addi RetVal ZeroReg i
                 , P.Jr RetAddr ]
  --V.Return retVal -> setupReturn retVal loadReg
  where
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
      (M M1, [ P.Lw (M M1) (k vreg) Sp ])

    arrAssignI_ :: Int -> [P.MipsPhys]
    arrAssignI_ 0 = [P.Sw (M M1) (Imm 0) (M M2)]
    arrAssignI_ i = arrAssignIV (i - 1) ++ [P.Sw (M M1) (Imm (show (i * 4))) (M M2)]



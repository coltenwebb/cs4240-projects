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

  V.Mult d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Sp
    ]

  V.Div d s t ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M2) (k t) Sp
    , P.Mult (M M1) (M M2)
    , P.Mflo (M M1)
    , P.Sw (M M1) (k d) Sp
    ]

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

  -- TODO: Handle branches

  V.Lw t i s ->
    [ P.Lw (M M1) (k s) Sp
    , P.Lw (M M1) i (M M1)
    , P.Sw (M M1) (k t) Sp
    ]

  V.Sw t i s ->
    [ P.Lw (M M1) (k s) Sp
    , P.Sw (M M2) i (M M1)
    , P.Sw (M M2) (k t) Sp
    ]

  V.Label lab -> [P.Label lab]

  V.Goto lab -> setupGoto lab

  -- TODO: Fix
  --V.Call fn args -> setupCallStack fn args loadReg

  --V.Callr retReg fn args ->
  --  setupCallStack fn args loadReg
  --    ++ [ P.Sw Retval (k retReg) Sp]

  --V.Return retVal -> setupReturn retVal loadReg
  where
    -- This is safe because genRegMap has assigned
    -- every virtual register its own unique index
    k :: VReg -> Imm
    k = Imm . show . unOffsetIdx . (M.!) rm

    loadReg :: VReg -> (PReg, [P.MipsPhys])
    loadReg vreg =
      (M M1, [ P.Lw (M M1) (k vreg) Sp ])


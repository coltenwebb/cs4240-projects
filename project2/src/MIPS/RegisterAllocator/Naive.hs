module MIPS.RegisterAllocator.Naive where

import MIPS.Types.Operands
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V

import qualified Data.Map as M
import Data.Foldable

newtype OffsetIdx = OffsetIdx Int
-- Location of virtual register on stack
-- $sp+offset*4, starting from offset=0
type RegMap = M.Map VReg OffsetIdx

genRegMap :: [VReg] -> RegMap
genRegMap = fst . foldl' f (mempty, 0)
  where
    f (c, idx) vr = (c <> M.singleton vr (OffsetIdx idx), idx+1)

virtToPhysMIPS
  :: RegMap
  -> MipsVirtual
  -> [MipsPhys]
virtToPhysMIPS rm mv = case mv of
  V.Add d s t ->
    [ Lw (M M1) (k s) Sp
    , Lw (M M2) (k t) Sp
    , Add (M M1) (M M1) (M M2)
    , Sw (M M1) (k d) Sp
    ]

  V.Sub vr vr' vr2 -> _
  V.Mult vr vr' vr2 -> _
  V.Div vr vr' vr2 -> _
  V.Andi vr vr' imm -> _
  V.And vr vr' vr2 -> _
  V.Ori vr vr' imm -> _
  V.Or vr vr' vr2 -> _
  V.Beq vr vr' lab -> _
  V.Bne vr vr' lab -> _
  V.Bgtz vr lab -> _
  V.Lw vr imm vr' -> _
  V.Sw vr imm vr' -> _
  V.Label lab -> _
  V.Goto lab -> _
  V.Call vr lab vrs -> _
  V.Return vr -> _
  where
  -- This is safe because genRegMap has assigned
  -- every virtual register its own unique index
  k :: VReg -> Imm
  k = Imm . show . (M.!) rm
module MIPS.CallingConvention where

import qualified Data.Map as M
import Data.Maybe

import TigerIR.Program

import MIPS.Types.Operand
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V

import Data.List (foldl1')

newtype OffsetIdx  = OffsetIdx  Int
newtype OffsetSize = OffsetSize Int

-- Offset addition
(.+) :: OffsetIdx -> OffsetSize -> OffsetIdx
(.+) (OffsetIdx i) (OffsetSize sz) = OffsetIdx (i + sz)

-- Size addition
(^+) :: OffsetSize -> OffsetSize -> OffsetSize
(^+) (OffsetSize sz1) (OffsetSize sz2) = OffsetSize (sz1 + sz2)

-- Offset subtraction
(.-) :: OffsetIdx -> OffsetSize -> OffsetIdx
(.-) (OffsetIdx i) (OffsetSize sz) = OffsetIdx (i - sz)

-- Offset Size Multiplication
(.*) :: OffsetSize -> Int -> OffsetSize
(.*) (OffsetSize sz) k = OffsetSize (sz * k)

-- Location of virtual register on stack
-- $sp+offset*4, starting from offset=0
type RegMap = M.Map VReg OffsetIdx

{-
hi addr
        ----------------------
  fp ->     local var m' - 1    PREVIOUS STACK FRAME
  ***** ---------------------- *******************
              arg 0             CURRENT STACK FRAME BEGIN
        ----------------------
                ...
        ----------------------
              arg n-1          CALLER setup, see `setupCallStack`
  ***** ---------------------- *******************
            local var 0        CALLEE setup
        ----------------------
                ...
        ----------------------  after callee allocates local var on stack
  sp ->      local var m-1     /    CURRENT STACK FRAME END
        ---------------------- *********************

lo addr
-}
calcRegMap :: TigerIrFunction -> RegMap
calcRegMap = _

initVarSize :: InitVar -> OffsetSize
initVarSize iv = case iv of
  InitV (Variable _) -> OffsetSize 4
  InitA (Array _ (ArraySize k)) -> OffsetSize 4 .* k

netVarOffset :: TigerIrFunction -> OffsetSize
netVarOffset
  (TigerIrFunction _  _ (Parameters pv) (LocalVars lvs) _) = paramSize ^+ lvarSize
  where
    paramSize :: OffsetSize
    paramSize = foldl1' (^+) (map initVarSize pv)

    lvarSize :: OffsetSize
    lvarSize = foldl1' (^+) (map initVarSize lvs)

setupCallStack
  :: Label
  -> [VReg]
  -- load value of virtual reg. onto physical reg
  -> (VReg -> (PReg, [P.MipsPhys]))
  -> [P.MipsPhys]
setupCallStack fn args loadReg =
  -- Save registers
  [ P.Sw  (T T0)  (Imm "0") Sp
  , P.Sw  (T T1)  (Imm "4") Sp
  , P.Sw  (T T2)  (Imm "8") Sp
  , P.Sw  (T T3)  (Imm "12") Sp
  , P.Sw  (T T4)  (Imm "16") Sp
  , P.Sw  (T T5)  (Imm "20") Sp
  , P.Sw  (T T6)  (Imm "24") Sp
  , P.Sw  (T T7)  (Imm "28") Sp
  , P.Sw  RetAddr (Imm "32") Sp
  , P.Sw  Fp      (Imm "36") Sp
  ]
  ++
  pushArgs
  ++
  [ P.Add Fp Sp ZeroReg            -- fp points to old sp
  , P.Addi Sp Sp spOffset          -- move sp up
  , P.Jal fn                       -- Jump
  -- Callee returned, teardown / restoring registers
  , P.Add Sp Fp ZeroReg            -- Restore sp

  , P.Lw  (T T0)  (Imm "0") Sp     -- Mirror save registers
  , P.Lw  (T T1)  (Imm "4") Sp
  , P.Lw  (T T2)  (Imm "8") Sp
  , P.Lw  (T T3)  (Imm "12") Sp
  , P.Lw  (T T4)  (Imm "16") Sp
  , P.Lw  (T T5)  (Imm "20") Sp
  , P.Lw  (T T6)  (Imm "24") Sp
  , P.Lw  (T T7)  (Imm "28") Sp
  , P.Lw  RetAddr (Imm "32") Sp
  , P.Lw  Fp      (Imm "36") Sp
  ]
  where
    pushArgs :: [P.MipsPhys]
    pushArgs = flip concatMap (zip [1..] args) $ \(argno, vreg) ->
      let offset = Imm . show . (+36) . (*4) $ argno
          (preg, loadInstrs) = loadReg vreg
      in loadInstrs ++ [P.Sw preg offset Sp]

    spOffset :: Imm
    spOffset = Imm . show $
      36                  -- saved registers
      + (4 * length args) -- fn args
      + 4                 -- points to above last arg

setupGoto :: Label -> [P.MipsPhys]
setupGoto lab = [P.J lab]

-- Setup instructions to return to caller
setupReturn ::
  Maybe VReg
  -> (VReg -> (PReg, [P.MipsPhys]))
  -> [P.MipsPhys]
setupReturn retVal loadReg =
  case retVal of
    Nothing -> [P.Jr RetAddr]
    Just rv ->
      let (preg, ins) = loadReg rv
      in
        ins ++
          [ P.Add Retval preg ZeroReg
          , P.Jr RetAddr
          ]

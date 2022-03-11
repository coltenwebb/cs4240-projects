module MIPS.CallingConvention where

import qualified Data.Map as M
import Data.Maybe

import TigerIR.Program

import MIPS.Types.Operand
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V

import Data.List (foldl1', foldl')

newtype OffsetIdx  = OffsetIdx  Int
newtype OffsetSize = OffsetSize Int

-- Location of virtual register on stack
-- $sp+offset*4, starting from offset=0
type RegMap = M.Map VReg OffsetIdx

-- OffsetIdx addition
-- Move pointer such that for
--  ix2 = ix1 .+ sz
-- [ix1 .. ix2] inclusive is a range of size sz
-- OffsetSize should never be negative!
(.+) :: OffsetIdx -> OffsetSize -> OffsetIdx
(.+) (OffsetIdx i) (OffsetSize sz) = OffsetIdx (i + sz - 1)

-- Size addition
(^+) :: OffsetSize -> OffsetSize -> OffsetSize
(^+) (OffsetSize sz1) (OffsetSize sz2) = OffsetSize (sz1 + sz2)

-- Offset subtraction
(.-) :: OffsetIdx -> OffsetSize -> OffsetIdx
(.-) (OffsetIdx i) (OffsetSize sz) = OffsetIdx (i - sz + 1)

-- Offset Size Multiplication
(.*) :: OffsetSize -> Int -> OffsetSize
(.*) (OffsetSize sz) k = OffsetSize (sz * k)

incrPtr :: OffsetIdx -> OffsetIdx
incrPtr (OffsetIdx i) = OffsetIdx (i + 1)

decrPtr :: OffsetIdx -> OffsetIdx
decrPtr (OffsetIdx i) = OffsetIdx (i - 1)

toImm :: OffsetIdx -> Imm
toImm (OffsetIdx i) = Imm (show (i * 4))


{-
hi addr
        ----------------------
  fp ->     local var m' - 1    PREVIOUS STACK FRAME
  ***** ---------------------- *******************
 ptr1 ->        arg 0              CURRENT STACK FRAME BEGIN
        ----------------------
                ...              (Corresponds to ParamVar)
        ----------------------
              arg n-1          CALLER setup, see `setupCallStack`
        ----------------------
              retAddr
        ----------------------
            caller's fp
  ***** ---------------------- *******************
 ptr2 ->     local var 0        CALLEE setup
        ----------------------
                ...
        ----------------------  after callee allocates local var on stack
  sp ->      local var m-1     /  CURRENT STACK FRAME END
        ---------------------- *********************

lo addr
-}
calcRegMap :: TigerIrFunction -> RegMap
calcRegMap fn =
  M.union
    -- arg0 to argn offsets
    ((fst . foldl' f1 (mempty, ptr1)) pvs)

    -- local var offsets
    ((fst . foldl' f2 (mempty, ptr2)) lvs)
  where
    -- See stack diagram above
    ptr1, ptr2 :: OffsetIdx
    ptr1 = OffsetIdx 0 .+ netVarSz
    ptr2 = OffsetIdx 0 .+ netLocalSz

    -- f1 and f2 are traversing downvards from hi to lo,
    -- down the stack, hence decrPtr and .- operations
    f1 :: (RegMap, OffsetIdx) -> ParamVar -> (RegMap, OffsetIdx)
    f1 (rm, offst) p =
      (rm <> M.singleton (toVReg p) offst, decrPtr offst)

    f2 :: (RegMap, OffsetIdx) -> LocalVar -> (RegMap, OffsetIdx)
    f2 (rm, offst) lv =
      (rm <> M.singleton (toVReg lv) currRegOffst, decrPtr currRegOffst)
      where
        currRegOffst = offst .- localVarSize lv

    netVarSz, netLocalSz, netParamSz :: OffsetSize
    netVarSz = netLocalSz ^+ netParamSz ^+ OffsetSize 2 -- add two to handle caller's retaddr and fp
    netLocalSz = netLocalVarSize fn
    netParamSz = netParamSize fn

    pvs :: Parameters
    pvs = parameters fn

    lvs :: LocalVars
    lvs = localVars fn

-- Because if type is an array, passed by reference
paramVarSize :: OffsetSize
paramVarSize = OffsetSize 1

localVarSize :: LocalVar -> OffsetSize
localVarSize iv = case iv of
  LocalV (Variable _) -> OffsetSize 1
  LocalA (Array _ (ArraySize k)) -> OffsetSize k

netLocalVarSize :: TigerIrFunction -> OffsetSize
netLocalVarSize fn = foldl1' (^+) (map localVarSize lvs)
  where
    lvs :: LocalVars
    lvs = localVars fn

netParamSize :: TigerIrFunction -> OffsetSize
netParamSize fn = paramVarSize .* length pvs
  where
    pvs :: Parameters
    pvs = parameters fn

-- TODO: put stack diagram
setupCallStack
  :: Label
  -> [VReg]
  -- load value of virtual reg. onto physical reg
  -> (VReg -> (PReg, [P.MipsPhys]))
  -> [P.MipsPhys]
setupCallStack fn args loadReg =
  -- Save registers
  [ P.Sw   (T T0)  (Imm "-4") Sp
  , P.Sw   (T T1)  (Imm "-8") Sp
  , P.Sw   (T T2)  (Imm "-12") Sp
  , P.Sw   (T T3)  (Imm "-16") Sp
  , P.Sw   (T T4)  (Imm "-20") Sp
  , P.Sw   (T T5)  (Imm "-24") Sp
  , P.Sw   (T T6)  (Imm "-28") Sp
  , P.Sw   (T T7)  (Imm "-32") Sp
  , P.Sw   RetAddr (Imm "-36") Sp
  , P.Sw   Fp      (Imm "-40") Sp
  , P.Addi Sp      Sp         (Imm "-40")
  , P.Add  Fp      Sp         Zero
  ]
  ++
  pushArgs
  ++
  [ P.Addi Sp Sp spOffset          -- move sp down
  , P.Jal fn                       -- Jump
  -- Callee returned, teardown / restoring registers
  , P.Add Sp Fp ZeroReg            -- Restore sp
  , P.Lw  Fp      (Imm "0")  Sp
  , P.Lw  RetAddr (Imm "4")  Sp
  , P.Lw  (T T7)  (Imm "8")  Sp
  , P.Lw  (T T6)  (Imm "12")  Sp
  , P.Lw  (T T5)  (Imm "16")  Sp
  , P.Lw  (T T4)  (Imm "20")  Sp
  , P.Lw  (T T3)  (Imm "24")  Sp
  , P.Lw  (T T2)  (Imm "28")  Sp
  , P.Lw  (T T1)  (Imm "32")  Sp
  , P.Lw  (T T0)  (Imm "36") Sp
  , P.Addi Sp     Sp        (Imm "40")
  ]
  where
    pushArgs :: [P.MipsPhys]
    pushArgs = flip concatMap (zip [1..] args) $ \(argno, vreg) ->
      let offset = Imm . show . (*(-4)) $ argno
          (preg, loadInstrs) = loadReg vreg
      in loadInstrs ++ [P.Sw preg offset Fp]

    spOffset :: Imm
    spOffset = Imm . show $
      - (4 * length args)  -- fn args (sp points to last arg)

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


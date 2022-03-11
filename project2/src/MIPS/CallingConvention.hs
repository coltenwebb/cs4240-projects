module MIPS.CallingConvention where

import qualified Data.Map as M
import Data.Maybe

import TigerIR.Program

import MIPS.Types.Operand
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V

import Data.List (foldl1', foldl')

-- Offset w.r.t. Frame pointer Fp
newtype OffsetIdx  = OffsetIdx  Int
newtype OffsetSize = OffsetSize Int

-- Location of virtual register on stack
-- $sp+offset*4, starting from offset=0
type RegMap = M.Map VReg OffsetIdx

-- OffsetIdx addition
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

incrPtr :: OffsetIdx -> OffsetIdx
incrPtr (OffsetIdx i) = OffsetIdx (i + 1)

decrPtr :: OffsetIdx -> OffsetIdx
decrPtr (OffsetIdx i) = OffsetIdx (i - 1)

toImm :: OffsetIdx -> Imm
toImm (OffsetIdx i) = Imm (show (i * 4))


{-
hi addr
        ----------------------
                ...
            saved registers     PREVIOUS STACK FRAME
                ...
        ----------------------
  fp ->        old_fp
  ***** ---------------------- *******************
 ptr1 ->        arg 0              CURRENT STACK FRAME BEGIN
        ----------------------
                ...              (Corresponds to ParamVar)
        ----------------------
              arg n-1          CALLER setup, see `setupCallStack`
  ***** ---------------------- *******************
 ptr2 ->     local var 0        CALLEE setup
        ----------------------
                ...
        ----------------------  after callee allocates local var on stack
  sp ->      local var m-1     /  CURRENT STACK FRAME END
        ---------------------- *********************

lo addr

case if local var is an array
hi
        ----------------------
          prev local vars...
        ----------------------
              arr[2]
        ----------------------
              arr[1]
        ----------------------
              arr[0]           <-- Where RegMap[arr] points to
        ----------------------
               ...
        ---------------------- 
lo

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
    ptr1 = OffsetIdx (-1)
    ptr2 = ptr1 .- netParamSize fn

    -- f1 and f2 are traversing downvards from hi to lo,
    -- down the stack, hence decrPtr and .- operations
    f1 :: (RegMap, OffsetIdx) -> ParamVar -> (RegMap, OffsetIdx)
    f1 (rm, offst) p =
      (rm <> M.singleton (toVReg p) offst, decrPtr offst)

    f2 :: (RegMap, OffsetIdx) -> LocalVar -> (RegMap, OffsetIdx)
    f2 (rm, offst) lv =
      (rm <> M.singleton (toVReg lv) currRegOffst, decrPtr currRegOffst)
      where
        currRegOffst = (offst .- localVarSize lv) .+ OffsetSize 1

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

{-
hi addr 
  old_sp ->
        ---------------------- *******************
                t0
        ----------------------
                ...
        ----------------------
                t7
        ----------------------
                $ra
        ----------------------
  fp ->         $old_fp         <- old_sp - 40
  ***** ---------------------- *******************
               arg 0             `pushArgs`
        ----------------------
                ...              
        ----------------------
              arg n-1           <- sp before Jal
  ***** ---------------------- *******************
            local var 0         Not handled by `setupCallStack`
        ----------------------  CALLEE setup
           more local vars...

lo addr
-}
setupCallStack
  :: Label
  -> [V.CallArg]
  -- load value of virtual reg. onto physical reg
  -> (VReg -> (PReg, [P.MipsPhys]))
  -> [P.MipsPhys]
setupCallStack fn args loadReg =
  -- Save registers
  [ P.Sw   (T T0)  (Imm "-4")  Sp
  , P.Sw   (T T1)  (Imm "-8")  Sp
  , P.Sw   (T T2)  (Imm "-12") Sp
  , P.Sw   (T T3)  (Imm "-16") Sp
  , P.Sw   (T T4)  (Imm "-20") Sp
  , P.Sw   (T T5)  (Imm "-24") Sp
  , P.Sw   (T T6)  (Imm "-28") Sp
  , P.Sw   (T T7)  (Imm "-32") Sp
  , P.Sw   RetAddr (Imm "-36") Sp
  , P.Sw   Fp      (Imm "-40") Sp
  , P.Addi Sp      Sp         (Imm "-40")
  , P.Add  Fp      Sp         ZeroReg
  ]
  ++
  pushArgs
  ++
  [ P.Addi Sp Sp (Imm (show (- (4 * length args)))) -- Move sp after pushing args
  , P.Jal fn                                      -- Call subroutine
  , P.Addi Sp Sp (Imm (show (4 * length args)))     -- Undo Move sp
  ]
  ++
  -- Callee returned, teardown / restoring registers
  [ P.Addi Sp Sp (Imm "40")
  , P.Lw   Fp      (Imm "-40") Sp
  , P.Lw   RetAddr (Imm "-36") Sp
  , P.Lw   (T T7)  (Imm "-32") Sp
  , P.Lw   (T T6)  (Imm "-28") Sp
  , P.Lw   (T T5)  (Imm "-24") Sp
  , P.Lw   (T T4)  (Imm "-20") Sp
  , P.Lw   (T T3)  (Imm "-16") Sp
  , P.Lw   (T T2)  (Imm "-12") Sp
  , P.Lw   (T T1)  (Imm "-8")  Sp
  , P.Lw   (T T0)  (Imm "-4")  Sp
  ]
  where
    pushArgs :: [P.MipsPhys]
    pushArgs = flip concatMap (zip [1..] args) $ \(argno, arg) ->
      let offset = Imm . show . (*(-4)) $ argno
      in 
        -- NOTE: Sp and Fp are the same at this point in time
        case arg of
          V.CVarg vreg ->
            let (preg, loadInstrs) = loadReg vreg
            in loadInstrs ++ [P.Sw preg offset Fp]
          V.CIarg i ->
            [ P.Li (M M1) i
            , P.Sw (M M1) offset Fp
            ]

setupGoto :: Label -> [P.MipsPhys]
setupGoto lab = [P.J lab]

-- Setup instructions to return to caller
setupReturn
  :: VReg
  -> (VReg -> (PReg, [P.MipsPhys]))
  -> [P.MipsPhys]
setupReturn retVal loadReg =
      let (preg, ins) = loadReg retVal
      in
        ins ++
          [ P.Add Retval preg ZeroReg
          , P.Jr RetAddr
          ]

setupReturnImm
  :: Imm
  -> [P.MipsPhys]
setupReturnImm retImm = 
  [ P.Li Retval retImm
  , P.Jr RetAddr
  ]


setupReturnVoid :: [P.MipsPhys]
setupReturnVoid = [P.Jr RetAddr]
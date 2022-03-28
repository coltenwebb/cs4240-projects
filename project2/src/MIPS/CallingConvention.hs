module MIPS.CallingConvention where

import qualified Data.Map as M
import Data.Maybe
import Control.Monad (when, forM_)

import TigerIR.Program

import MIPS.Types.Operand
import MIPS.RegisterAllocator.Monad.Class
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V
import MIPS.Intrinsics

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


-- Stack/Fp setup upon entry into function
fnEntry :: MonadAllocator m => Function a -> m ()
fnEntry fn = do
  -- Fp <- Sp a bit of a necessary hack, bc. we are using our own
  -- calling convention since fp is zero-initialized
  when (name fn == FunctionName (Label "main")) $
    emit [ P.Add Fp Sp ZeroReg ]
  
  -- Move stack pointer to accomodate local vars
  emit [ P.Addi Sp Sp (toImm offst) ]

  -- Alloc arrays via sbrk
  allocArrays fn

  where
    offst = OffsetIdx 0 .- netLocalVarSize fn

{- HLINT ignore "Use lambda-case" -}
allocArrays :: MonadAllocator m => Function a -> m ()
allocArrays fn =
  forM_ (localVars fn) $ \lvar ->
    case lvar of
      LocalV _ -> pure ()
      LocalA (Array v (ArraySize sz)) -> do
        offst <- getStackOffsetImm (VReg v)
        emit [ loadSyscall Sbrk
             , P.Li (A A0) (Imm (show sz))
             , P.Syscall
             , P.Sw V0 offst Fp
             ]

{-
hi addr
        ----------------------
                ...
         PREVIOUS STACK FRAME
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

-}
calcRegMap :: Function a -> RegMap
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
      (rm <> M.singleton (toVReg lv) offst, decrPtr offst)

    pvs :: Parameters
    pvs = parameters fn

    lvs :: LocalVars
    lvs = localVars fn

-- All params and variables are the same size,
-- array vars are references to memory
paramVarSize :: OffsetSize
paramVarSize = OffsetSize 1

-- array vars also refs in local var
localVarSize :: OffsetSize
localVarSize = OffsetSize 1

netLocalVarSize :: Function a -> OffsetSize
netLocalVarSize fn = paramVarSize .* length lvs
  where
    lvs :: LocalVars
    lvs = localVars fn

netParamSize :: Function a -> OffsetSize
netParamSize fn = paramVarSize .* length pvs
  where
    pvs :: Parameters
    pvs = parameters fn

{-

hi addr 
  old_sp ->
        ---------------------- *******************
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
  :: MonadAllocator m
  => Label
  -> [V.CallArg]
  -> m ()
--  -- load value of virtual reg. onto physical reg
--  -> (VReg -> (PReg, [P.MipsPhys]))
--  -> [P.MipsPhys]
setupCallStack fn args = do
  -- Save registers
  emit
    [ P.Sw   RetAddr (Imm "-4") Sp
    , P.Sw   Fp      (Imm "-8") Sp
    , P.Addi Sp      Sp         (Imm "-8")
    ]
 
  pushArgs

  emit
    [ P.Add  Fp Sp ZeroReg -- Fp cannot be moved until args pushed
    , P.Addi Sp Sp (Imm (show (- (4 * length args)))) -- Move sp after pushing args
    , P.Jal fn                                        -- Call subroutine
    , P.Add Sp Fp ZeroReg
    ]
  
  -- Callee returned, teardown / restoring registers
  emit
    [ P.Addi Sp Sp (Imm "8")
    , P.Lw   Fp      (Imm "-8") Sp
    , P.Lw   RetAddr (Imm "-4") Sp
    ]
  where
    pushArgs :: MonadAllocator m => m ()
    pushArgs = forM_ (zip [1..] args) $ \(argno, arg) -> do
      let offset = Imm . show . (* (-4)) $ argno
      -- IMPORTANT NOTE: Sp and Fp are the same at this point in time
      case arg of
        V.CVarg vreg ->
          regs_x vreg $ \preg -> emit [ P.Sw preg offset Sp ]
        V.CIarg i ->
          regs_tmp $ \tmp -> emit [ P.Li tmp i, P.Sw tmp offset Sp ]
  
setupGoto :: MonadMipsEmitter m => Label -> m ()
setupGoto lab = emit [P.J lab]

-- Setup instructions to return to caller
setupReturn :: MonadAllocator m => VReg -> m ()
setupReturn retVal = regs_x retVal $ \r -> 
  emit [ P.Add Retval r ZeroReg , P.Jr RetAddr ]

setupReturnImm :: MonadMipsEmitter m => Imm -> m ()
setupReturnImm retImm = emit [ P.Li Retval retImm , P.Jr RetAddr ]

setupReturnVoid :: MonadMipsEmitter m => m ()
setupReturnVoid = emit [P.Jr RetAddr]
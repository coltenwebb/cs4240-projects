module MIPS.RegisterAllocator.Allocation where

import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V
import MIPS.Types.Operand
import MIPS.RegisterAllocator.Monad.Class
import MIPS.CallingConvention
import TigerIR.Program
import TigerIR.IrInstruction as T

import Control.Monad (forM_)

virtToEmitPhysMIPS
  :: MonadAllocator m
  => Function a
  -> V.MipsVirtual
  -> m ()
virtToEmitPhysMIPS fn mv = case mv of
  V.Addi d s i -> regs_dx d s $ \d' s' ->
    emit [P.Addi d' s' i]

  V.Add d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [P.Add d' s' t']
  
  V.Sub d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [P.Sub d' s' t']
  
  V.SubVI d s i -> regs_dxi d s i $ \d' s' i' ->
    emit [P.Sub d' s' i']
  
  V.SubIV d i s -> regs_dix d i s $ \d' i' s' ->
    emit [P.Sub d' i' s']
  
  V.Mult d s t -> regs_dxy d s t $ \d' s' t' -> 
    emit [ P.Mult s' t', P.Mflo d' ]
  
  V.Multi d s i -> regs_dxi d s i $ \d' s' i' ->
    emit [ P.Mult s' i', P.Mflo d' ]
  
  V.Div d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [ P.Div s' t', P.Mflo d' ]

  V.DivVI d s i -> regs_dxi d s i $ \d' s' i' ->
    emit [ P.Div s' i', P.Mflo d' ]
  
  V.DivIV d i s -> regs_dix d i s $ \d' i' s' ->
    emit [ P.Div i' s', P.Mflo d' ]
  
  V.Andi d s i -> regs_dx d s $ \d' s' ->
    emit [ P.Andi d' s' i ]
  
  V.And d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [ P.And d' s' t' ]

  V.Ori d s i -> regs_dx d s $ \d' s' ->
    emit [ P.Ori d' s' i ]
  
  V.Or d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [ P.Or d' s' t' ]

  V.BrVV c a b lbl -> case c of
    T.Breq  -> regs_xy a b $ \a' b' -> emit [ P.Beq a' b' lbl ]

    T.Brneq -> regs_xy a b $ \a' b' -> emit [ P.Bne a' b' lbl ]

    -- a < b <===> 0 < b - a
    T.Brlt  -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Bgtz tmp lbl ]

    -- a > b <===> a - b > 0
    T.Brgt  -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Bgtz tmp lbl ]

    -- a >= b <===> 0 >= b - a
    T.Brgeq -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Blez tmp lbl ]

    -- a <= b <===> a - b <= 0
    T.Brleq -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Blez tmp lbl ]

  V.BrVI c a bi lbl -> case c of
    T.Breq  -> regs_xi a bi $ \a' b' -> emit [ P.Beq a' b' lbl ]

    T.Brneq -> regs_xi a bi $ \a' b' -> emit [ P.Bne a' b' lbl ]

    -- a < b <===> 0 < b - a
    T.Brlt  -> regs_xi_tmp a bi $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Bgtz tmp lbl ]

    -- a > b <===> a - b > 0
    T.Brgt  -> regs_xi_tmp a bi $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Bgtz tmp lbl ]

    -- a >= b <===> 0 >= b - a
    T.Brgeq -> regs_xi_tmp a bi $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Blez tmp lbl ]

    -- a <= b <===> a - b <= 0
    T.Brleq -> regs_xi_tmp a bi $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Blez tmp lbl ]
  
  V.BrIV c ai b lbl -> case c of
    Breq  -> regs_ix ai b $ \a' b' -> emit [ P.Beq a' b' lbl ]

    T.Brneq -> regs_ix ai b $ \a' b' -> emit [ P.Bne a' b' lbl ]

    -- a < b <===> 0 < b - a
    T.Brlt  -> regs_ix_tmp ai b $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Bgtz tmp lbl ]

    -- a > b <===> a - b > 0
    T.Brgt  -> regs_ix_tmp ai b $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Bgtz tmp lbl ]

    -- a >= b <===> 0 >= b - a
    T.Brgeq -> regs_ix_tmp ai b $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Blez tmp lbl ]

    -- a <= b <===> a - b <= 0
    T.Brleq -> regs_ix_tmp ai b $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Blez tmp lbl ]
  
  V.BrII c ai bi lbl -> case c of
    T.Breq -> regs_ii ai bi $ \a' b' -> emit [ P.Beq a' b' lbl ]

    T.Brneq -> regs_ii ai bi $ \a' b' -> emit [ P.Bne a' b' lbl ]

    -- a < b <===> 0 < b - a
    T.Brlt -> regs_ii_tmp ai bi $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Bgtz tmp lbl ]

    -- a > b <===> a - b > 0
    T.Brgt -> regs_ii_tmp ai bi $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Bgtz tmp lbl ]

    -- a >= b <===> 0 >= b - a
    T.Brgeq -> regs_ii_tmp ai bi $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Blez tmp lbl ]

    -- a <= b <===> a - b <= 0
    T.Brleq -> regs_ii_tmp ai bi $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Blez tmp lbl ]
  
  V.Label lab -> emit [ P.Label lab ]

  V.Goto lab -> setupGoto lab

  V.Li vr im -> regs_assign_di vr im

  V.Call fn args -> setupCallStack fn args

  V.Callr retReg fn args -> do
    setupCallStack fn args
    regs_d retReg $ \dest ->
  -- Could be wasted op if we will be directly Sw-ing
  -- onto stack anyways, but necessary for
  -- MonadAllocator generality. Guess you can't have
  -- your cake and eat it too.
      emit [ P.Add dest Retval ZeroReg ]
  
  V.Assign d s -> regs_dx d s $ \d' s' ->
    emit [ P.Add d' s' ZeroReg ]
  
  V.ArrStrVV valR arr offstR -> regs_xyz_tmp valR arr offstR $
    \val' arr' offst' tmp -> emit
      [ P.Sll  tmp  offst'   (Imm "2")       -- tmp <- offst' << 2 (offst' * 4)
      , P.Add  tmp  tmp       arr'           -- tmp <- tmp + arr'  (ptr. arithm)
      , P.Sw   val' (Imm "0") tmp            -- mem[tmp+0] = val'
      ]

  V.ArrStrIV valI arr offstR -> regs_xyi_tmp arr offstR valI $
    \arr' offstR' val' tmp -> emit           -- same logic as V.ArrStr above
      [ P.Sll tmp  offstR'   (Imm "2")
      , P.Add tmp  tmp       arr'
      , P.Sw  val' (Imm "0") tmp
      ]
  
  V.ArrStrVI valR arr offstI -> regs_xyi arr valR (immTimes4 offstI) $
    \arr' val' offst4I' -> emit
    -- It is safe to overwrite here because offst' is for imm value
      [ P.Add offst4I' offst4I' arr'
      , P.Sw  val'    (Imm "0") offst4I'
      ]
  
  V.ArrStrII valI arr offstI -> regs_xii arr valI (immTimes4 offstI) $
    \arr' val' offst4I' -> emit
      [ P.Add offst4I' offst4I'  arr'
      , P.Sw  val'   (Imm "0") offst4I'
      ]
  
  V.ArrLoadV d arr offstR -> regs_dxy_tmp d arr offstR $
    \d' arr' offstR' tmp -> emit
      [ P.Sll tmp offstR'   (Imm "2")
      , P.Add tmp tmp       arr'
      , P.Lw  d'  (Imm "0") tmp
      ]
  
  V.ArrLoadI d arr offstI -> regs_dxi d arr (immTimes4 offstI) $
    \d' arr' offst4I' -> emit
      [ P.Add offst4I' offst4I' arr'     -- also safe to overwrite
      , P.Lw d' (Imm "0") offst4I'
      ]
  
  V.ArrAssignV arr (Imm size) valR -> regs_xy arr valR $
    \arr' val' -> do
      let n = (read size :: Int)
      forM_ [0..n-1] $ \i ->
        emit [ P.Sw val' (Imm (show i)) arr' ]
  
  V.ArrAssignI arr (Imm size) valI -> regs_xi arr valI $
    \arr' val' -> do -- same as V.ArrAssignV
      let n = (read size :: Int)
      forM_ [0..n-1] $ \i ->
        emit [ P.Sw val' (Imm (show i)) arr' ]
  
  V.Nop -> pure ()

  V.Return vreg -> setupReturn vreg

  V.Returni imm -> setupReturnImm imm

  V.BeginFunction -> fnEntry fn

  V.EndFunction -> setupReturnVoid
  
  where
    immTimes4 :: Imm -> Imm
    immTimes4 (Imm i) = Imm . show . (* 4) $ (read i :: Int)

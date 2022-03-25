module MIPS.RegisterAllocator.Allocation where

import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V
import MIPS.RegisterAllocator.Monad.Class
import MIPS.CallingConvention (setupCallStack)

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
  
  V.DivIV d i s -> regs_dix d s i $ \d' s' i' ->
    emit [ P.Div i' s', P.Mflo d' ]
  
  V.Andi d s i -> regs_dx (d, s) $ \d' s' ->
    emit [ P.Andi d' s' i ]
  
  V.And d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [ P.And d' s' t' ]

  V.Ori d s i -> regs_dx d s $ \d' s' ->
    emit [ P.Ori d' s' i ]
  
  V.Or d s t -> regs_dxy d s t $ \d' s' t' ->
    emit [ P.Or s' t' ]
  
  V.Br c a b lbl -> case c of
    V.Eq  -> regs_xy a b $ \a' b' -> emit [ P.Beq a' b' lbl ]
    V.Neq -> regs_xy a b $ \a' b' -> emit [ P.Bne a' b' lbl ]

    -- a < b <===> 0 < b - a
    V.Lt  -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Bgtz tmp lbl ]

    -- a > b <===> a - b > 0
    V.Gt  -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Bgtz tmp lbl ]

    -- a >= b <===> 0 >= b - a
    V.Geq -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp b' a', P.Blez tmp lbl ]

    -- a <= b <===> a - b <= 0
    V.Leq -> regs_xy_tmp a b $ \a' b' tmp ->
      emit [ P.Sub tmp a' b', P.Blez tmp lbl ]
  
  V.Label lab -> emit [ P.Label lab ]

  V.Goto lab -> emit $ setupGoto lab

  V.Call fn args -> setupCallStack fn args

  V.Callr retReg fn args -> do
    setupCallStack fn args
    regs_d retReg $ \dest ->
  -- Could be wasted op if we will be directly Sw-ing
  -- onto stack anyways, but necessary for
  -- MonadAllocator generality. Guess you can't have
  -- your cake and eat it too.
      emit [ P.Add dest Retval ZeroReg ]
  
  -- Same issue as Callr with extraneous load
  V.AssignI d i -> regs_assign_di d i

  V.AssignV d s -> regs_dx $ \d' s' ->
    emit [ P.Add d' s' ZeroReg ]
  
  V.ArrStrVV valR arr offstR -> regs_xyz_tmp valR arr offstR $
    \val' arr' offst' tmp -> emit
      [ P.Sll  tmp  offst'   (Imm "2")       -- tmp <- offst' << 2 (offst' * 4)
      , P.Add  tmp  tmp       arr'           -- tmp <- tmp + arr'  (ptr. arithm)
      , P.Sw   val' (Imm "0") tmp            -- mem[tmp+0] = val'
      ]
  
  V.ArrStrIV valI arr offstR -> regs_xyi_tmp arr offstR valI $
    \arr' offst' val' tmp -> emit           -- same logic as V.ArrStr above
      [ P.Sll tmp  offst'    (Imm "2")
      , P.Add tmp  tmp       arr'
      , P.Sw  val' (Imm "0") tmp
      ]
  
  V.ArrStrVI valR arr offstI -> regs_xyi arr valR offstI $
    \arr' val' offst' -> emit  -- It is safe to overwrite here because offst' is for imm value
      [ P.Sll offst' offst'   (Imm "2")
      , P.Add offst' offst'    arr'
      , P.Sw  val'   (Imm "0") tmp
      ]


  


  _ -> _ -- TODO !!! :)
  
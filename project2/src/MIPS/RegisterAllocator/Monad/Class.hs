{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module MIPS.RegisterAllocator.Monad.Class where

import qualified MIPS.Types.Virtual  as V
import qualified MIPS.Types.Physical as P
import MIPS.Types.Operand

import Control.Monad.Writer.Class
import Data.DList as D

type MipsPhysDList = D.DList P.MipsPhys

class (MonadWriter MipsPhysDList m) => MonadMipsEmitter m where
  emit :: [P.MipsPhys] -> m ()
  emit = tell . D.fromList

class (MonadMipsEmitter m, Monad m) => MonadAllocator m where

  getStackOffsetImm :: VReg -> m Imm

  loadVRegFromStack :: VReg -> PReg -> m ()
  loadVRegFromStack vreg preg = do
    offsetImm <- getStackOffsetImm vreg
    emit [ P.Lw preg offsetImm Fp ]

  saveVRegToStack :: VReg -> PReg -> m ()
  saveVRegToStack vreg preg = do
    offsetImm <- getStackOffsetImm vreg
    emit [ P.Sw preg offsetImm Fp ]

  loadImmediate :: Imm -> PReg -> m ()
  loadImmediate imm preg = emit [ P.Li preg imm ]

  getUniqueCounter :: m Int

  -- Get corresponding physical registers from virtual registers,
  -- then apply callback that emits corresponding instructions.

  -- dest d, reg x, and reg y
  regs_dxy
    :: VReg -> VReg -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()

  -- dest d, reg x
  regs_dx
    :: VReg -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> m' ())
    -> m ()

  -- reg x, reg y
  regs_xy
    :: VReg -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> m' ())
    -> m ()

  -- dest d, reg x, reg holding imm val
  regs_dxi
    :: VReg -> VReg -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()

  -- reg x, reg y, register to hold tmp value
  regs_xy_tmp
    :: VReg -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()
  
  -- reg x, reg y, reg z, register to hold tmp value
  regs_xyz_tmp
    :: VReg -> VReg -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> PReg -> m' ())
    -> m ()
  
  -- dest d 
  regs_d
    :: VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> m' ())
    -> m ()

  -- reg x, reg y, load imm value into tmp reg
  regs_xyi
    :: VReg -> VReg -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()

  -- load register x
  regs_x
    :: VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> m' ())
    -> m ()
  
  -- temp register to hold intermediate value
  regs_tmp
    :: (forall m'. MonadMipsEmitter m' => PReg -> m' ())
    -> m ()
  
  -- Asign imm value to dest reg
  regs_assign_di
    :: VReg -> Imm
    -> m ()

  -- Reg X, Load two immediate values to tmp
  regs_xii
    :: VReg -> Imm -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()

  -- dest d, reg x, reg y, tmp register to hold intermediate value
  regs_dxy_tmp
    :: VReg -> VReg -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> PReg -> m' ())
    -> m ()
  
  -- Reg x, Reg y, Imm i, Tmp
  regs_xyi_tmp
    :: VReg -> VReg -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> PReg -> m' ())
    -> m ()

  -- Reg x, Imm i
  regs_xi
    :: VReg -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> m'())
    -> m ()
  
  -- Two registers to hold two immediate values
  regs_ii
    :: Imm -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> m'())
    -> m ()
  
  -- Two registers to hold two immediate values, one tmp
  regs_ii_tmp
    :: Imm -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m'())
    -> m ()
  
  -- Reg x, Imm value i, and one tmp
  regs_xi_tmp
    :: VReg -> Imm
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m'())
    -> m ()

  ----------------------------- 
  -- End of Minimal Definition
  -----------------------------

  -- dest d, reg holding imm val, reg x
  regs_dix
    :: VReg -> Imm -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()
  regs_dix d i x callback = regs_dxi d x i $
    \d' x' i' -> callback d' i' x'

  regs_ix_tmp
    :: Imm -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> PReg -> m' ())
    -> m ()
  regs_ix_tmp i v callback = regs_xi_tmp v i $
    \v' i' -> callback i' v'

  regs_ix
    :: Imm -> VReg
    -> (forall m'. MonadMipsEmitter m' => PReg -> PReg -> m' ())
    -> m ()
  regs_ix i x callback = regs_xi x i $
    \x' i' -> callback i' x'
  

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module MIPS.RegisterAllocator.Monad.NaiveM where

import MIPS.RegisterAllocator.Monad.Class
import MIPS.CallingConvention
import MIPS.Types.Operand
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual as V

import Control.Monad.Writer
import Control.Monad.RWS.Lazy

import qualified Data.Map as M
import MIPS.RegisterAllocator.Monad.Class

type NaiveM = RWS RegMap MipsPhysDList ()
instance MonadMipsEmitter NaiveM

-- assuming that RegMap was generated correctly,
-- i.e. contains every valid VReg, in our use of (M.!)
getOffsetImm :: VReg -> NaiveM Imm
getOffsetImm v = reader (toImm . (M.! v))

loadVRegFromStack :: VReg -> PReg -> NaiveM ()
loadVRegFromStack vreg preg = do
  offsetImm <- getOffsetImm vreg
  emit [ P.Lw preg offsetImm Fp ]

saveVRegToStack :: VReg -> PReg -> NaiveM ()
saveVRegToStack vreg preg = do
  offsetImm <- getOffsetImm vreg
  emit [ P.Sw preg offsetImm Fp ]

instance MonadAllocator NaiveM where
  regs_dxy d x y callback = do
    let (d', x', y') = (M M1, M M1, M M2)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback d' x' y'
    saveVRegToStack d d'
  
  regs_dx d x callback = do
    let (d', x') = (M M1, M M1)

    loadVRegFromStack x x'
    callback d' x'
    saveVRegToStack d d'

  regs_xy x y callback = do
    let (x', y') = (M M1, M M2)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback x' y'
  
  regs_dxi d x i callback = do
    let (d', x', i') = (M M1, M M1, M M2)

    loadVRegFromStack x x'
    emit [ P.Li i' i ]
    callback d' x' i'
    saveVRegToStack d d'
  
  regs_xy_tmp x y callback = do
    -- T registers aren't used in naive
    let (x', y', tmp) = (M M1, M M2, T T0)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback x' y' tmp
  
  regs_d d callback = do
    let d' = M M1

    callback d'
    saveVRegToStack d d'

  regs_x x callback = do
    let x' = M M1

    loadVRegFromStack x x'
    callback x'

  regs_xyz_tmp callback = do
    let (x', y', z', tmp) = (M M1, M M2, M M3, M M4)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    loadVRegFromStack z z'
    callback x' y' z' tmp
  
  regs_tmp callback = do
    let tmp = M M1
    callback tmp
  
  regs_assign_di d i = do
    let tmp = M M1
    
    emit [ P.Li tmp i ]
    saveVRegToStack d tmp

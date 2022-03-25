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
  regs_dxy (d, x, y) callback = do
    let (d', x', y') = (M M1, M M1, M M2)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback (d', x', y')
    saveVRegToStack d d'
  
  regs_dx (d, x) callback = do
    let (d', x') = (M M1, M M1)

    loadVRegFromStack x x'
    callback (d', x')
    saveVRegToStack d d'
  
  regs_dxi (d, x, i) callback = do
    let (d', x', i') = (M M1, M M1, M M2)

    loadVRegFromStack x x'
    emit [ P.Li i' i ]
    callback (d', x', i')
    saveVRegToStack d d'
  
  regs_d d callback = do
    let d' = M M1

    callback d'
    saveVRegToStack d d'

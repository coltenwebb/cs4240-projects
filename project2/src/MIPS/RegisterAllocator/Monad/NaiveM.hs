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
import Data.DList as D
import qualified Data.Map as M

type UniqueCounter = Int
type NaiveM = RWS RegMap MipsPhysDList UniqueCounter
instance MonadMipsEmitter NaiveM

runNaiveM :: NaiveM a -> RegMap -> [P.MipsPhys]
runNaiveM nm regmap = D.toList pinsts
  where
    (_, _, pinsts) = runRWS nm regmap 0

instance MonadAllocator NaiveM where
  -- assuming that RegMap was generated correctly,
  -- i.e. contains every valid VReg, in our use of lookup
  getStackOffsetImm v = reader (\mp -> toImm (mp M.! v))
  
  getUniqueCounter = do
    n <- get
    put $ n + 1
    return n

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
    loadImmediate i i'
    callback d' x' i'
    saveVRegToStack d d'

  regs_xy_tmp x y callback = do
    -- T registers aren't used in naive
    let (x', y', tmp) = (M M1, M M2, M M3)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback x' y' tmp

  regs_d d callback = do
    let d' = M M1

    callback d'
    saveVRegToStack d d'

  regs_xyi x y i callback = do
    let (x', y', i') = (M M1, M M2, M M3)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    loadImmediate i i'
    callback x' y' i'

  regs_x x callback = do
    let x' = M M1

    loadVRegFromStack x x'
    callback x'

  regs_xyz_tmp x y z callback = do
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

    loadImmediate i tmp
    saveVRegToStack d tmp

  regs_xii x i1 i2 callback = do
    let (x', i1', i2') = (M M1, M M2, M M3)

    loadVRegFromStack x x'
    loadImmediate i1 i1'
    loadImmediate i2 i2'
    callback x' i1' i2'

  regs_dxy_tmp d x y callback = do
    let (d', x', y', tmp) = (M M1, M M2, M M3, M M4)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback d' x' y' tmp
    saveVRegToStack d d'

  regs_xyi_tmp x y i callback = do
    let (x', y', i', tmp) = (M M1, M M2, M M3, M M4)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    loadImmediate i i'
    callback x' y' i' tmp

  regs_xi x i callback = do
    let (x', i') = (M M1, M M2)

    loadVRegFromStack x x'
    loadImmediate i i'
    callback x' i'

  regs_xi_tmp x i callback = do
    let (x', i', tmp) = (M M1, M M2, M M3)

    loadVRegFromStack x x'
    loadImmediate i i'
    callback x' i' tmp

  regs_ii i1 i2 callback = do
    let (i1', i2') = (M M1, M M2)

    loadImmediate i1 i1'
    loadImmediate i2 i2'
    callback i1' i2'

  regs_ii_tmp i1 i2 callback = do
    let (i1', i2', tmp) = (M M1, M M2, M M3)

    loadImmediate i1 i1'
    loadImmediate i2 i2'
    callback i1' i2' tmp

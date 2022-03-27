{-# LANGUAGE FlexibleInstances #-}
module MIPS.RegisterAllocator.Monad.GreedyM where

import MIPS.RegisterAllocator.Monad.Class
import MIPS.RegisterAllocator.Greedy.Graph
import MIPS.CallingConvention
import MIPS.Types.Operand
import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual as V

import Control.Monad.Writer
import Control.Monad.RWS.Lazy
import Data.DList as D
import qualified Data.Map as M

type GreedyM =
  RWS (RegMap, ColorLookup) MipsPhysDList BasicBlockLine

-- IMPORTANT!!!1!111!: This must be run on the scope of
-- a single Basic Block only, because it depends on an internal
-- state counter of BasicBlockLine
runGreedyM :: GreedyM a -> RegMap -> ColorLookup -> [P.MipsPhys]
runGreedyM greedy rm cl = D.toList pinsts
  where
    -- IMPORTANT 2!!!: BBL is 1-indexed, see Greedy.Graph
    (_, _, pinsts) = runRWS greedy (rm, cl) (BBL 1)

incrBBL :: GreedyM ()
incrBBL = modify (\(BBL n) -> BBL (n+1))

tryTmpReg :: VReg -> GreedyM (Maybe TmpReg)
tryTmpReg vreg = do
  currBBL <- get
  reader (\(_, cl) -> lookupColor cl vreg currBBL)

-- Attempt to allocate register, if not colored,
-- use the given spillover reg
-- Bool: reg is spilled (for if we need to save)
allocReg :: VReg -> PReg -> GreedyM PReg
allocReg vreg spillover = do
  reg <- tryTmpReg vreg
  case reg of
    Just x -> pure $ T x
    Nothing -> do
      loadVRegFromStack vreg spillover
      pure spillover

-- Does not actually load from stack, since
-- the preg could be currently in use until
-- the end of the instr. i.e due to line_num+/-
allocDestReg :: VReg -> PReg -> GreedyM PReg
allocDestReg  vreg spillover = do
  reg <- tryTmpReg vreg
  case reg of
    Just x  -> pure $ T x
    Nothing -> pure spillover

instance MonadMipsEmitter GreedyM

instance MonadAllocator GreedyM where
  -- if (M.!) fails, it means we screwed up the regmap,
  -- same as in NaiveM oof
  getStackOffsetImm v = reader (\(mp, _) -> toImm (mp M.! v))

  regs_dxy d x y callback = do
    d' <- allocDestReg d (M M1)
    x' <- allocReg     x (M M2)
    y' <- allocReg     y (M M3)

    callback d' x' y'

    saveVRegToStack d d'

  regs_dx d x callback = do
    let (d', x') = (T T1, T T1)

    loadVRegFromStack x x'
    callback d' x'
    saveVRegToStack d d'

  regs_xy x y callback = do
    let (x', y') = (T T1, T T2)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback x' y'

  regs_dxi d x i callback = do
    let (d', x', i') = (T T1, T T1, T T2)

    loadVRegFromStack x x'
    loadImmediate i i'
    callback d' x' i'
    saveVRegToStack d d'

  regs_xy_tmp x y callback = do
    -- T registers aren't used in naive
    let (x', y', tmp) = (T T1, T T2, T T3)

    loadVRegFromStack x x'
    loadVRegFromStack y y'
    callback x' y' tmp

  regs_d d callback = return ()

  regs_xyi x y i callback = return ()

  regs_xyz_tmp x y z callback = return ()

  regs_x x callback = return ()

  regs_tmp callback = return ()

  regs_assign_di d i = do
    d' <- allocDestReg d (M M1)
    loadImmediate i d'
    saveVRegToStack d d'

  regs_xii x i1 i2 callback = do
    x' <- allocReg x (M M1)
    let (i1', i2') = (M M2, M M3)
    loadImmediate i1 i1'
    loadImmediate i2 i2'
    callback x' i1' i2'

  regs_dxy_tmp d x y callback = do
    d' <- allocDestReg d (M M1)
    x' <- allocReg     x (M M2)
    y' <- allocReg     y (M M3)
    let tmp = M M4
    callback d' x' y' tmp
    saveVRegToStack d d'

  regs_xyi_tmp x y i callback = do
    x' <- allocReg x (M M1)
    y' <- allocReg y (M M2)
    let (i', tmp) = (M M3, M M4)
    loadImmediate i i'
    callback x' y' i' tmp

  regs_xi x i callback = do
    x' <- allocReg x (M M1)
    let i' = M M2
    loadImmediate i i'
    callback x' i'

  regs_xi_tmp x i callback = do
    x' <- allocReg x (M M1)
    let (i', tmp) = (M M2, M M3)
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

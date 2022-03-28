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

newtype UniqueCounter = U Int
type CurrRegMapping = M.Map TmpReg VReg
type GreedyM =
  RWS
    (RegMap, ColorLookup)
    MipsPhysDList
    (BasicBlockLine, CurrRegMapping, UniqueCounter)

fst' (a,_,_)  = a
snd' (_,b,_)  = b
thr' (_,_,c)  = c

-- IMPORTANT!!!1!111!: This must be run on the scope of
-- a single Basic Block only, because it depends on an internal
-- state counter of BasicBlockLine
-- UniqueCounter also needs to be unique across the entire
-- function!!!!! not just basic block
runGreedyM
  :: UniqueCounter
  -> GreedyM a
  -> RegMap
  -> ColorLookup
  -> (UniqueCounter, [P.MipsPhys])
runGreedyM uc greedy rm cl = (ucAfter, D.toList pinsts)
  where
    -- IMPORTANT 2!!!: BBL is 1-indexed, see Greedy.Graph
    (_, st, pinsts) = runRWS greedy (rm, cl) (BBL 1, mempty, uc)
    (_, _, ucAfter) = st

incrBBL :: GreedyM ()
incrBBL = modify (\(BBL n, k, i) -> (BBL (n+1), k, i))

getCurrRegMapping :: TmpReg -> GreedyM (Maybe VReg)
getCurrRegMapping tr = do
  crm <- snd' <$> get
  return $ M.lookup tr crm

tryTmpReg :: VReg -> GreedyM (Maybe TmpReg)
tryTmpReg vreg = do
  currBBL <- fst' <$> get
  reader (\(_, cl) -> lookupColor cl vreg currBBL)

-- Attempt to allocate register, if not colored,
-- use the given spillover reg
-- Bool: reg is spilled (for if we need to save)
allocReg :: VReg -> PReg -> GreedyM PReg
allocReg vreg spillover = do
  reg <- tryTmpReg vreg
  case reg of
    Just t -> do
      currVreg <- getCurrRegMapping t
      when (currVreg /= Just vreg) $ loadVRegFromStack vreg (T t)
      pure $ T t

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

markMapping :: VReg -> PReg -> GreedyM ()
markMapping vreg preg = do
  case preg of
    T tmpReg -> modify (\(bbl, mp, n) -> (bbl, M.insert tmpReg vreg mp, n))
    _        -> pure ()

saveVRegToStackAndMark :: VReg -> PReg -> GreedyM ()
saveVRegToStackAndMark v p =
  saveVRegToStack v p >> markMapping v p

instance MonadMipsEmitter GreedyM

instance MonadAllocator GreedyM where
  -- if (M.!) fails, it means we screwed up the regmap,
  -- same as in NaiveM oof
  getStackOffsetImm v = reader (\(mp, _) -> toImm (mp M.! v))

  getUniqueCounter = do
    (x, y, U n) <- get
    put (x, y, U (n+1))
    return n

  regs_dxy d x y callback = do
    d' <- allocDestReg d (M M1)
    x' <- allocReg     x (M M2)
    y' <- allocReg     y (M M3)

    callback d' x' y'
    saveVRegToStackAndMark d d'

  regs_dx d x callback = do
    d' <- allocDestReg d (M M1)
    x' <- allocReg     x (M M2)

    callback d' x'
    saveVRegToStackAndMark d d'

  regs_xy x y callback = do
    x' <- allocReg x (M M1)
    y' <- allocReg y (M M2)

    callback x' y'

  regs_dxi d x i callback = do
    d' <- allocDestReg d (M M1)
    x' <- allocReg     x (M M2)
    let i' = M M3
    loadImmediate i i'

    callback d' x' i'
    saveVRegToStackAndMark d d'

  regs_xy_tmp x y callback = do
    x'       <- allocReg x (M M1)
    y'       <- allocReg y (M M2)
    let tmp' = M M3

    callback x' y' tmp'

  regs_d d callback = do
    d' <- allocDestReg d (M M1)

    callback d'
    saveVRegToStackAndMark d d'

  regs_xyi x y i callback = do
    x' <- allocReg x (M M1)
    y' <- allocReg y (M M2)
    let i' = M M3
    loadImmediate i i'
    callback x' y' i'

  regs_x x callback = do
    x' <- allocReg x (M M1)
    callback x'

  regs_xyz_tmp x y z callback = do
    x' <- allocReg x (M M1)
    y' <- allocReg y (M M2)
    z' <- allocReg z (M M3)
    let tmp = M M4

    callback x' y' z' tmp

  regs_tmp callback = do
    let tmp = M M1
    callback tmp

  regs_assign_di d i = do
    d' <- allocDestReg d (M M1)
    loadImmediate i d'
    saveVRegToStackAndMark d d'

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
    saveVRegToStackAndMark d d'

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

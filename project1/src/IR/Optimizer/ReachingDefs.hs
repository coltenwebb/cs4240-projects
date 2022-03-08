{-# LANGUAGE FlexibleContexts #-}
module IR.Optimizer.ReachingDefs where

import IR.Instruction
import IR.Optimizer.CFG

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Control.Monad.State.Lazy
import Control.Monad.Reader

import Debug.Trace

-- so we need a way to represent the cfg
-- we will have a list of basic blocks
-- each basic block will be a list of instructions itself
--
-- the textbook gives pseudocode pg 241
newtype GenSet  = GenSet  { unGenSet  :: S.Set Instruction } deriving Show
newtype KillSet = KillSet { unKillSet :: S.Set Instruction } deriving Show
newtype InSet   = InSet   { unInSet   :: S.Set Instruction } deriving (Eq, Show)
newtype OutSet  = OutSet  { unOutSet  :: S.Set Instruction } deriving (Eq, Show)

type GenSets  = M.Map BlockId GenSet
type KillSets = M.Map BlockId KillSet

data ReachDefSets = ReachDefSets
  { currInSets  :: M.Map BlockId InSet
  , currOutSets :: M.Map BlockId OutSet
  , setsChanged :: Bool
  , iteration   :: Int
  }

type CFG' = CFG Instruction

data Env = Env
  { cfg   :: CFG'
  , gens  :: M.Map BlockId GenSet
  , kills :: M.Map BlockId KillSet
  }

data ReachDefResult = ReachDefResult
  { inSets   :: M.Map BlockId InSet
  , outSets  :: M.Map BlockId OutSet
  , genSets  :: M.Map BlockId GenSet
  , killSets :: M.Map BlockId KillSet
  } deriving Show

newtype WriteMap = WriteMap {unWriteMap :: M.Map Variable [Instruction]}

updateInAndOuts :: (MonadReader Env m, MonadState ReachDefSets m) => m ()
updateInAndOuts = do
  Env cfg' gens kills <- ask
  let bbs = getBasicBlocks cfg'
  forM_ bbs $
    \bb -> modify $ iterGenInOutSetSingleBlock bb cfg' gens kills

reachingDefAlgorithm
  :: (MonadReader Env m, MonadState ReachDefSets m)
  => m ReachDefResult
reachingDefAlgorithm = do
  loopUntilFixedPoint updateInAndOuts

  Env _ g k <- ask
  ReachDefSets i o _ iterCnt <- get
  --return $ trace ("iterCnt: " ++ show iterCnt) ReachDefResult i o g k
  return $ ReachDefResult i o g k
  where
    loopUntilFixedPoint m = do
      m
      fixedPointsReached <- not . setsChanged <$> get
      unless fixedPointsReached $ do
        modify $ \s -> s { setsChanged = False, iteration = iteration s + 1 }
        loopUntilFixedPoint m

runReachingDefAlgorithm :: CFG' -> ReachDefResult
runReachingDefAlgorithm cfg' = evalState (runReaderT reachingDefAlgorithm env) initState
  where
    bbs = getBasicBlocks cfg'

    -- Lecture 3, pg. 17, by definition of Gen[S] and Kill[S]
    g = M.fromList . map (\bb -> (blockId bb, genGenSet  bb))      $ bbs
    k = M.fromList . map (\bb -> (blockId bb, genKillSet bb cfg')) $ bbs

    -- Lecture 3. pg 30, re: how to initialize in & outs
    i = M.fromList . map (\bb -> (blockId bb, InSet  mempty))      $ bbs
    o = M.fromList . map (\bb -> (blockId bb, OutSet mempty))      $ bbs

    env = Env cfg' g k
    initState = ReachDefSets i o True 1

genGenSet :: BasicBlock -> GenSet
genGenSet bb = GenSet $ S.fromList . M.elems . foldl' f mempty $ bInstrs
  where
    -- The fold ensures that subsequent definitions kill/overwrite
    -- previous definitions in the same block
    f :: M.Map Variable Instruction
      -> Instruction
      -> M.Map Variable Instruction
    f gens ins = case defVars ins of
      Nothing -> gens
      Just vari -> M.insert vari ins gens

    bInstrs = NE.toList . instrs $ bb

genKillSet :: BasicBlock -> CFG' -> KillSet
genKillSet bb@(BasicBlock ins _ blkId) (CFG lkup _ _) = killS
  where
    ins' = NE.toList ins

    -- Piazza @53 follow-up, it is ok to not include
    -- killed defs from the current block, as long
    -- as we don't include the killed defs into the gen set
    otherIns = [ins | (bId, bb) <- M.toList lkup,
                      bId /= blkId,
                      ins <- NE.toList (instrs bb),
                      isDefOpcode (opcode ins)]

    -- Could be more efficient by only adding
    -- `defs` in the current basic block instead of all defs.
    -- Too lazy tho lol
    wmap :: WriteMap
    wmap = genWriteMap otherIns

    defs :: [Variable]
    defs = mapMaybe defVars ins'

    killS = KillSet . S.fromList . concat $
      mapMaybe (`M.lookup` unWriteMap wmap) defs

-- Lecture 3, Page 32
iterGenInOutSetSingleBlock
  :: BasicBlock
  -> CFG'
  -> GenSets
  -> KillSets
  -> ReachDefSets
  -> ReachDefSets
iterGenInOutSetSingleBlock bb cfg gens kills
  rds@(ReachDefSets prevIns prevOuts chngd iter) = nxtState
  where
    blkId = blockId bb

    predIds :: [BlockId]
    predIds = map blockId $ predecessors bb cfg

    genB, killB, nxtInB, nxtOutB :: S.Set Instruction
    genB  = maybe mempty unGenSet  (M.lookup blkId gens)
    killB = maybe mempty unKillSet (M.lookup blkId kills)

    -- Lecture 3, pg 17.
    nxtInB = S.unions . map unOutSet $ mapMaybe (`M.lookup` prevOuts) predIds
    nxtOutB = genB `S.union` (nxtInB S.\\ killB)

    hasChanged = chngd || (M.lookup blkId prevIns /= Just (InSet nxtInB))
      || (M.lookup blkId prevOuts /= Just (OutSet nxtOutB))

    nxtInSets  = M.insert blkId (InSet nxtInB) prevIns
    nxtOutSets = M.insert blkId (OutSet nxtOutB) prevOuts

    nxtState = ReachDefSets nxtInSets nxtOutSets hasChanged iter

genWriteMap :: [Instruction] -> WriteMap
genWriteMap ins = WriteMap $ M.fromListWith (++) allVarInstPairs
  where
  varInstPairs inst = fmap (\v -> (v, [inst])) $ defVars inst 
  allVarInstPairs = mapMaybe varInstPairs ins

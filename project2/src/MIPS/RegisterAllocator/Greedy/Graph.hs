{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module MIPS.RegisterAllocator.Greedy.Graph where

import MIPS.RegisterAllocator.Greedy.MipsCFG
import MIPS.Types.Operand
import qualified MIPS.Types.Virtual as V

import Control.Monad.State.Lazy
import Control.Monad (forM_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.List (sortOn)
import qualified Data.Ord
import Data.Maybe


import Debug.Trace

newtype BasicBlockLine = BBL Int deriving (Ord, Eq, Num, Show)
newtype UseCount       = UC  Int deriving (Ord, Eq, Num, Show)

data LiveRange = LiveRange
  { defVar :: VReg
  -- where the last def was
  , defBegin :: BasicBlockLine
  -- live range ends on the last use before next def
  , useEnd   :: BasicBlockLine
  , useCount :: UseCount
  } deriving (Ord, Eq, Show)

interferes :: LiveRange -> LiveRange -> Bool
interferes
  (LiveRange _ (BBL b1) (BBL e1) _)
  (LiveRange _ (BBL b2) (BBL e2) _)
    -- b_i < e_j is strict ineq, because e_j corresponds
    -- to a use, so it technically is bbl-
    -- and b_i is a def on bbl+.
    -- i.e when b_i == e_j, we have e_j = bbl-, b_i = bbl+
    -- for ex:
    -- (bbl 5)   add, x, a, b
    --    def of x does not interfere with use of a
    --    if this is the end of the live range of a
    --    and beginning of the live range of x
    = (b1 <= b2 && b2 < e1) || (b2 <= b1 && b1 < e2)

data BuildState = BuildState
  { earliestDef :: M.Map VReg BasicBlockLine
  , prevUse     :: M.Map VReg BasicBlockLine
  , useCounter  :: M.Map VReg UseCount
  , currRanges  :: [LiveRange]
  }

type BuildStateM = State BuildState

iterBbIns :: (BasicBlockLine, V.MipsVirtual) -> BuildStateM ()
iterBbIns (bbl, mv) = do
  let uses = V.getUses mv

  forM_ uses $ \usedVReg -> do
    cnter <- useCounter <$> get
    let cnter' = M.insertWith (+) usedVReg (UC 1) cnter

    prevU <- prevUse <$> get
    let prevU' = M.insert usedVReg bbl prevU

    modify (\bs -> bs { useCounter = cnter', prevUse = prevU' })

  let def = V.getDef mv
  case def of
    Nothing -> pure ()
    Just defVReg -> when (defVReg `notElem` uses) $ do
      -- beginning of new live range, insert old
      defLine <- M.findWithDefault (BBL (-1)) defVReg . earliestDef <$> get
      useLine <- M.findWithDefault defLine  defVReg . prevUse  <$> get
      useCnt  <- M.findWithDefault (UC 0) defVReg . useCounter <$> get

      let newLR = LiveRange defVReg defLine useLine useCnt
      currRs  <- currRanges <$> get
      modify (\bs -> bs { currRanges = newLR : currRs })

      -- Reset use counter
      useCnter <- useCounter <$> get
      modify (\bs -> bs { useCounter = M.insert defVReg (UC 0) useCnter })

      -- Remove previous use, we won't override the inserted uses, because
      -- defVReg is not in `uses`
      prevUseMap <- prevUse <$> get
      modify (\bs -> bs { prevUse = M.delete defVReg prevUseMap })

      -- Set new earliest def to current bblk line
      earliestDefMap <- earliestDef <$> get
      modify (\bs -> bs { earliestDef = M.insert defVReg bbl earliestDefMap })

-- insert remaining live ranges
handleFinish :: BuildStateM ()
handleFinish = do
  prevUseMap <- prevUse <$> get
  let vregs = M.keys prevUseMap
  forM_ vregs $ \vreg -> do
    defLine <- M.findWithDefault (BBL (-1)) vreg . earliestDef <$> get
    useLine <- M.findWithDefault defLine    vreg . prevUse     <$> get
    useCnt  <- M.findWithDefault (UC 0)     vreg . useCounter  <$> get
    
    let newLR = LiveRange vreg defLine useLine useCnt
    currRs  <- currRanges <$> get
    modify (\bs -> bs { currRanges = newLR : currRs })

buildLiveRanges :: BasicBlockMips -> [LiveRange]
buildLiveRanges bbm = currRanges $ execState iterAllIns initState
  where
    initState = BuildState mempty mempty mempty mempty

    mv = NE.toList . instrs $ bbm

    iterAllIns = do
      let bblMips = zip (map BBL [1..]) mv
      mapM_ iterBbIns bblMips
      handleFinish

--------------------------------------
-- Graph building and allocation logic
--------------------------------------
type LiveRangeGraph = M.Map LiveRange (S.Set LiveRange)
type LiveRangeColoring = M.Map LiveRange TmpReg
type ColoringState = State LiveRangeColoring

allocatableRegs :: S.Set TmpReg
allocatableRegs = S.fromList [T0 .. T7]

buildLiveRangeGraph :: [LiveRange] -> LiveRangeGraph
buildLiveRangeGraph lrs = M.fromListWith mappend
  [ (r1, S.singleton r2) | r1 <- lrs, r2 <- lrs, r1 `interferes` r2]

liveRangePrioList :: [LiveRange] -> [LiveRange]
liveRangePrioList = sortOn (Data.Ord.Down . useCount)

-- Registers occupied by adjacent live ranges
adjOccupiedRegs :: LiveRangeGraph -> LiveRange -> ColoringState (S.Set TmpReg)
adjOccupiedRegs lrg lr = do
  currColoring <- get
  let adjLrs = maybe [] S.toList (M.lookup lr lrg)
      occupied = mapMaybe (`M.lookup` currColoring) adjLrs

  return $ S.fromList occupied

-- It could be possible that no register available
getAvailReg :: LiveRangeGraph -> LiveRange -> ColoringState (Maybe TmpReg)
getAvailReg lrg lr = do
  occupied <- adjOccupiedRegs lrg lr
  let avail = allocatableRegs S.\\ occupied
  if null avail
    then pure Nothing
    else pure . Just $ S.findMin avail

colorRange1 :: LiveRangeGraph -> LiveRange -> ColoringState ()
colorRange1 lrg lr = do
  maybeReg <- getAvailReg lrg lr
  case maybeReg of
    Nothing -> pure ()
    Just reg -> do
      -- insert live range and corresp. reg into the coloring graph
      modify $ M.insert lr reg

execColorGreedy :: LiveRangeGraph -> [LiveRange] -> LiveRangeColoring
execColorGreedy lrg lrs = execState m mempty
  where
    prioLrs = liveRangePrioList lrs
    m = mapM_ (colorRange1 lrg) prioLrs

-- Convenience map data structure for coloring lookup
type ColorLookup = M.Map VReg (M.Map BasicBlockLine TmpReg)

buildColorLookup :: LiveRangeColoring -> ColorLookup
buildColorLookup lrc = execState m mempty
  where
    colorings = M.toList lrc

    m :: State ColorLookup ()
    m = do
      -- insert map entries for each vreg first
      forM_ colorings $ \(LiveRange vreg _ _ _, _) ->
        modify $ M.insert vreg mempty

      -- insert a tmp reg alloc for each line in each range
      forM_ colorings $ \(lr, tmpReg) -> do
        let (LiveRange vreg (BBL bgn) (BBL end) _) = lr
            newEntries = M.fromList [(BBL i, tmpReg) | i <- [bgn..end]]

        -- union with existing entries
        modify $ M.insertWith M.union vreg newEntries

lookupColor :: ColorLookup -> VReg -> BasicBlockLine -> Maybe TmpReg
lookupColor cl vr bbl =
  case M.lookup vr cl of
    Nothing -> Nothing
    Just mp -> M.lookup bbl mp

runGreedyColoring :: BasicBlockMips -> ColorLookup
runGreedyColoring bbm = colorLookup
  where
    liveRanges        = buildLiveRanges bbm
    liveRangeGraph    = buildLiveRangeGraph liveRanges
    liveRangeColoring = execColorGreedy liveRangeGraph liveRanges
    colorLookup       = buildColorLookup liveRangeColoring

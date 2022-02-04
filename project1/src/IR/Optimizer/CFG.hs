{-# LANGUAGE DerivingVia #-}

module IR.Optimizer.CFG where

import IR.Instruction

import Data.List as L (foldl', reverse, head)
import Data.List.NonEmpty as NE hiding (map)
import Data.Maybe (mapMaybe)
import Test.QuickCheck
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Monoid as S

-- TODO: switch to non-empty list
data BasicBlock = BasicBlock
  { instrs :: NonEmpty Instruction
  , lastIns :: Instruction
  , blockId :: BlockId
  } deriving Show
newtype BlockId = BlockId Int
  deriving (Eq, Ord, Show)


type CfgAdjMap = M.Map BlockId (S.Set BlockId)
newtype RevCfg = RevCfg CfgAdjMap deriving Show
data CFG = CFG
  { blockLookup :: M.Map BlockId BasicBlock
  , adjMap :: CfgAdjMap
  , revAdjMap :: RevCfg
  } deriving Show

getLeader :: BasicBlock -> Instruction
getLeader = NE.head . instrs
  -- { instrs :: [Instruction]
  -- , inSet :: S.Set Instruction
  -- , outSet :: S.Set Instruction
  -- , genSet :: S.Set Instruction
  -- , killSet :: S.Set Instruction
  -- }

getBasicBlocks :: CFG -> [BasicBlock]
getBasicBlocks = M.elems . blockLookup

splitIntoBasicBlocks :: [Instruction] -> [BasicBlock]
splitIntoBasicBlocks [] = []
splitIntoBasicBlocks (x:xs) = g . foldl f (x :| [] , [], 0) $ xs
  where
    -- Prepend for O(1) time
    appendIns currBlk ins = ins <| currBlk

    -- Reverse because we prepended
    toBlk blk i = BasicBlock (NE.reverse blk) (NE.head blk) (BlockId i)

    prevIns = NE.head

    -- Need to reverse because BasicBlock result list was also prepended
    g (lastBlk, res, cnt) = L.reverse $ toBlk lastBlk cnt : res

    -- We also process the prevIns in order to use NonEmpty list logic
    f (currBlk, res, cnt) currIns
      | isBranching (prevIns currBlk) = (currIns :| [], res', cnt+1)
      | isLabel currIns               = (currIns :| [], res', cnt+1)
      | otherwise                     = (currBlk `appendIns` currIns, res, cnt)
      where
        curr = prevIns currBlk
        res' = toBlk currBlk cnt : res


makeCFG :: [Instruction] -> CFG
makeCFG ins = CFG blkLookup adjMap (transposeMap adjMap)
  where
    blks = splitIntoBasicBlocks ins

    blkLookup = M.fromList . map (\b -> (blockId b, b)) $ blks

    labelLookup = labelMap blks

    adjMap = M.fromList [(blockId bb, S.fromList (blockEdges bb)) | bb <- blks]

    blockEdges :: BasicBlock -> [BlockId]
    blockEdges bb@(BasicBlock _ lst (BlockId i))
      | lastOp == RETURN = []
      | lastOp == GOTO = labelBlockId
      | lastOp `elem` branchOpcodes = adj ++ labelBlockId
      | otherwise = adj
      where
        lastOp = opcode lst

        adj = [BlockId (i+1) | BlockId (i+1) `M.member` blkLookup]

        -- Label blockId lookup logic
        getLabel (LabelOperand lname) = Just lname
        getLabel _                    = Nothing
        label                         = getLabel (L.head (operands lst))
        labelBlockId                  = case label of
                                          Nothing -> []
                                          Just l ->
                                            case l `M.lookup` labelLookup of
                                              Nothing -> [] -- If not GOTO or BRANCH
                                              Just bb -> [blockId bb]

    transposeMap :: CfgAdjMap -> RevCfg
    transposeMap cfg =
      RevCfg $ M.fromListWith S.mappend
        [(v, S.singleton k) | (k, s) <- M.toList cfg, v <- S.toList s]

successors :: BasicBlock -> CFG -> [BasicBlock]
successors bb (CFG blkLookup adjMap _) =
  case blockId bb `M.lookup` adjMap of
    Nothing -> []
    Just ids -> mapMaybe (`M.lookup` blkLookup) . S.toList $ ids

predecessors :: BasicBlock -> CFG -> [BasicBlock]
predecessors bb (CFG blockLookup _ (RevCfg revAdj)) =
  case blockId bb `M.lookup` revAdj of
    Nothing -> []
    Just ids -> mapMaybe (`M.lookup` blockLookup) . S.toList $ ids

labelMap :: [BasicBlock] -> M.Map LabelName BasicBlock
labelMap = foldl handle M.empty
  where
    handle mp blk@(BasicBlock ((Instruction _ (LabelOperand lbn:_) _):|_) _ _)
      = M.insert lbn blk mp
    handle mp _ = mp
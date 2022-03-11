{-# LANGUAGE DerivingVia #-}

module Optimizer.CFG where

import IR.Instruction

import Data.List as L (foldl', reverse, head)
import Data.List.NonEmpty as NE hiding (map)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid

class TigerIrInstruction a where
  isBranching    :: a -> Bool
  isLabel        :: a -> Bool
  isReturnOp     :: a -> Bool
  isGotoOp       :: a -> Bool
  isBranchOpcode :: a -> Bool
  -- If it is a branch / goto / etc. it will have a label
  -- to jump to, used for determining consecutive blocks
  getLabelOperand :: a -> Maybe LabelName

instance TigerIrInstruction Instruction where
  isBranching inst
    | opcode inst `elem` [
        BREQ, BRNEQ, BRLT, BRGT, BRGEQ, BRLEQ, GOTO, RETURN
      ] = True
    | otherwise = False

  isLabel = (== LABEL) . opcode

  isReturnOp = (== RETURN) . opcode

  isGotoOp = (== GOTO) . opcode

  isBranchOpcode = (`elem` branchOpcodes) . opcode

  getLabelOperand (Instruction _ ops _) = f (L.head ops)
    where
      f (LabelOperand lname) = Just lname
      f _ = Nothing
      


-- TODO: switch to non-empty list
data BasicBlockGeneral a = BasicBlock
  { instrs :: NonEmpty a
  , lastIns :: a
  , blockId :: BlockId
  } deriving Show

newtype BlockId = BlockId Int
  deriving (Eq, Ord, Show)

type BasicBlock = BasicBlockGeneral Instruction

type CfgAdjMap = M.Map BlockId (S.Set BlockId)
type BlockLookup a = M.Map BlockId (BasicBlockGeneral a)
newtype RevCfg = RevCfg CfgAdjMap deriving Show

data CFG a = CFG
  { blockLookup :: BlockLookup a
  , adjMap :: CfgAdjMap
  , revAdjMap :: RevCfg
  } deriving Show

getLeader :: BasicBlockGeneral a -> a
getLeader = NE.head . instrs
  -- { instrs :: [Instruction]
  -- , inSet :: S.Set Instruction
  -- , outSet :: S.Set Instruction
  -- , genSet :: S.Set Instruction
  -- , killSet :: S.Set Instruction
  -- }

getBasicBlocks :: CFG a -> [BasicBlockGeneral a]
getBasicBlocks = M.elems . blockLookup

splitIntoBasicBlocks :: (TigerIrInstruction a) => [a] -> [BasicBlockGeneral a]
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


makeCFG :: (TigerIrInstruction a) => [a] -> CFG a
makeCFG ins = CFG blkLookup adjMap (transposeMap adjMap)
  where
    blks = splitIntoBasicBlocks ins

    blkLookup = M.fromList . map (\b -> (blockId b, b)) $ blks

    labelLookup = labelToBlockIdMap blks

    adjMap = M.fromList [(blockId bb, S.fromList (blockEdges bb)) | bb <- blks]

    blockEdges :: (TigerIrInstruction a) => BasicBlockGeneral a -> [BlockId]
    blockEdges bb@(BasicBlock _ lst (BlockId i))
      | isReturnOp lst = []
      | isGotoOp lst = labelBlockId
      | isBranchOpcode lst = adj ++ labelBlockId
      | otherwise = adj
      where
        adj = [BlockId (i+1) | BlockId (i+1) `M.member` blkLookup]

        -- Label blockId lookup logic
        label                         = getLabelOperand lst
        labelBlockId                  = case label of
                                          Nothing -> []
                                          Just l ->
                                            case l `M.lookup` labelLookup of
                                              Nothing -> [] -- If not GOTO or BRANCH
                                              Just bId -> [bId]

    transposeMap :: CfgAdjMap -> RevCfg
    transposeMap cfg =
      RevCfg $ M.fromListWith mappend
        [(v, S.singleton k) | (k, s) <- M.toList cfg, v <- S.toList s]

successors :: BasicBlockGeneral a -> CFG a -> [BasicBlockGeneral a]
successors bb (CFG blkLookup adjMap _) =
  case blockId bb `M.lookup` adjMap of
    Nothing -> []
    Just ids -> mapMaybe (`M.lookup` blkLookup) . S.toList $ ids

predecessors :: BasicBlockGeneral a -> CFG a -> [BasicBlockGeneral a]
predecessors bb (CFG blockLookup _ (RevCfg revAdj)) =
  case blockId bb `M.lookup` revAdj of
    Nothing -> []
    Just ids -> mapMaybe (`M.lookup` blockLookup) . S.toList $ ids

labelToBlockIdMap
  :: (TigerIrInstruction a) => [BasicBlockGeneral a] -> M.Map LabelName BlockId
labelToBlockIdMap bbs = M.fromList
  [(lname, blockId bb) | bb <- bbs, 
                        let firstInstr = NE.head (instrs bb),
                        lname <- (maybeToList . getLabelOperand) firstInstr]
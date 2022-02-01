module IR.Optimizer.ReachingDefs where

import IR.Instruction
import IR.Optimizer.MarkSweep
import IR.Optimizer.CFG

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.List (foldl')
import Data.Maybe (mapMaybe)

import Control.Monad.State.Lazy

-- so we need a way to represent the cfg
-- we will have a list of basic blocks
-- each basic block will be a list of instructions itself
--
-- the textbook gives pseudocode pg 241
newtype GenSet  = GenSet  { unGenSet  :: S.Set Instruction }
newtype KillSet = KillSet { unKillSet :: S.Set Instruction }
newtype InSet   = InSet   { unInSet   :: S.Set Instruction } deriving Eq
newtype OutSet  = OutSet  { unOutSet  :: S.Set Instruction } deriving Eq

type GenSets  = M.Map BlockId GenSet
type KillSets = M.Map BlockId KillSet

data ReachDefSets = ReachDefSets
  { currInSets :: M.Map BlockId InSet
  , currOutSets :: M.Map BlockId OutSet
  , setsChanged :: Bool
  }

type ReachDefState = State ReachDefSets

-- Lecture 3. pg 30
initSets :: CFG -> GenSets -> KillSets -> State ReachDefSets ()
initSets cfg gens kills = put $ ReachDefSets i o False
  where
    i = undefined
    o = undefined

updateInAndOuts
  :: CFG
  -> GenSets
  -> KillSets
  -> State ReachDefSets ()
updateInAndOuts cfg g k = do
  s <- get
  -- TODO: should be iterating over every single basic block
  --forM (M.value)
  --let nxtState = iterGenInOutSetSingleBlock bb cfg g k s
  --put nxtState

reachingDefAlgorithm
  :: BasicBlock
  -> CFG
  -> State ReachDefSets ()
reachingDefAlgorithm = undefined
-- 1) init sets
-- 2) loop until setsChanged = False
-- 3) exit



genGenSet :: BasicBlock -> GenSet
genGenSet bb = GenSet $ S.fromList [k | k <- ins, isDefOpcode (opcode k)]
  where
    ins = NE.toList . instrs $ bb

genKillSet :: BasicBlock -> CFG -> KillSet
genKillSet bb@(BasicBlock ins _ blkId) (CFG lkup _ _) = killS
  where
    ins' = NE.toList ins

    otherIns = [ins | (bId, bb) <- M.toList lkup,
                      bId /= blkId,
                      ins <- NE.toList (instrs bb)]

    -- Could be more efficient by only adding
    -- `defs` in the current basic block instead of all defs.
    -- Too lazy tho lol
    wmap :: WriteMap
    wmap = genWriteMap otherIns

    defs :: [Variable]
    defs = concatMap defVars ins'

    killS = KillSet . S.fromList . concat $
      mapMaybe (`M.lookup` wmap) defs

-- Lecture 3, Page 32
iterGenInOutSetSingleBlock
  :: BasicBlock
  -> CFG
  -> GenSets
  -> KillSets
  -> ReachDefSets
  -> ReachDefSets
iterGenInOutSetSingleBlock bb cfg gens kills
  rds@(ReachDefSets prevIns prevOuts chngd) = nxtState
  where
    blkId = blockId bb

    predIds :: [BlockId]
    predIds = map blockId $ predecessors bb cfg

    genB, killB, nxtInB, nxtOutB :: S.Set Instruction
    genB  = maybe mempty unGenSet  (M.lookup blkId gens)
    killB = maybe mempty unKillSet (M.lookup blkId kills)
    nxtInB = S.unions . map unOutSet $ mapMaybe (`M.lookup` prevOuts) predIds
    nxtOutB = genB `S.union` (nxtInB S.\\ killB)

    hasChanged = chngd || (M.lookup blkId prevIns /= Just (InSet nxtInB))
      || (M.lookup blkId prevOuts /= Just (OutSet nxtOutB))

    nxtInSets  = M.insert blkId (InSet nxtInB) prevIns
    nxtOutSets = M.insert blkId (OutSet nxtOutB) prevOuts

    nxtState = ReachDefSets nxtInSets nxtOutSets setsChanged



--genGenSets :: [BasicBlock] -> [BasicBlock]
--genGenSets [] = []
--genGenSets (bb:bbs) = bb { genSet = g'} : genGenSets bbs
--  where
--    g' :: S.Set Instruction
--    g' = S.fromList [l | l <- unBasicBlock bb, opcode l `elem` defOpcodes]
--

--instance Show BasicBlock where
--  show bb =
--    "\n" ++ show (blockId bb) ++ "\n" ++
--    "    IN: " ++ show (S.map lineNum (inSet bb)) ++ "\n" ++
--    "    OUT: " ++ show (S.map lineNum (outSet bb)) ++ "\n" ++
--    "    GEN: " ++ show (S.map lineNum (genSet bb)) ++ "\n" ++
--    "    KILL: " ++ show (S.map lineNum (killSet bb)) ++ "\n" ++
--    "    insts[ " ++ show (map lineNum (unBasicBlock bb)) ++ "]\n"
--
--insertEveryOther :: Show a => [Char] -> [a] -> [Char]
--insertEveryOther _ [] = []
--insertEveryOther i (x:xs) = show x ++ i ++ insertEveryOther i xs
--
--isBranching :: Instruction -> Bool
--isBranching inst
--  | opcode inst `elem` [
--      BREQ, BRNEQ, BRLT, BRGT, BRGEQ, BRLEQ, GOTO, RETURN
--    ] = True
--  | otherwise = False
--
--
--createBasicBlocks :: [Instruction] -> [BasicBlock]
--createBasicBlocks = g . foldl' f ([], [])
--  where
--    g = undefined
--    
--    f (currBlk, res) ins
--      | null currBlk 
----createBasicBlocks insts = assignBlockId 0 (split insts)
--
--split :: [Instruction] -> [BasicBlock]
--split insts = foldl handle [] insts
--  where
--    handle :: [BasicBlock] -> Instruction -> [BasicBlock]
--    handle blks inst
--      | null blks = [createBB [inst]]
--      | isBranching (lastInst blks) = blks ++ [createBB [inst]]
--      | isLabel inst = blks ++ [createBB [inst]]
--      | otherwise = init blks ++ [createBB (unBasicBlock (last blks) ++ [inst])]
--    lastInst blks = last . unBasicBlock $ last blks
--    createBB :: [Instruction] -> BasicBlock
--    createBB insts = BasicBlock insts mempty mempty mempty mempty 0
--
--assignBlockId :: Int -> [BasicBlock] -> [BasicBlock]
--assignBlockId _ [] = []
--assignBlockId i blocks@(bb:bbx) = bb {blockId = i} : assignBlockId (i+1) bbx
--
--labelMap :: [BasicBlock] -> M.Map LabelName BasicBlock
--labelMap blks = foldl handle M.empty blks
--  where
--    handle mp blk@(BasicBlock ((Instruction _ (LabelOperand lbn:_) _):_) _ _ _ _ _) = M.insert lbn blk mp
--    handle mp _ = mp
--
--neighbors :: M.Map BasicBlock [BasicBlock]
--neighbors = undefined
--
---- row index represents src node, col index represents dest node
--adjacencyMatrix :: [BasicBlock] -> [[Int]]
--adjacencyMatrix blocks = map handle (zip [0..] blocks)
--  where
--    handle :: (Int, BasicBlock) -> [Int]
--    handle (i, bb)
--      | i == length blocks - 1      = replicate (length blocks) 0 
--      | lastOp == RETURN            = replicate (length blocks) 0 
--      | lastOp == GOTO              = [if x == head labelBlockId then 1 else 0 | x <- [0..length blocks - 1]]
--      | lastOp `elem` branchOpcodes = [if x `elem` (i+1) : labelBlockId then 1 else 0 | x <- [0..length blocks - 1]] 
--      | otherwise                   = [if x == i + 1 then 1 else 0 | x <- [0..length blocks - 1]]
--      where
--        lmap = labelMap blocks
--        lastInst                      = last (unBasicBlock bb)
--        lastOp                        = opcode lastInst
--        getLabel (LabelOperand lname) = Just lname
--        getLabel _                    = Nothing
--        label                         = getLabel (head (operands lastInst))
--        labelBlockId                  = case label of
--                                          Nothing -> []
--                                          Just l -> 
--                                            case l `M.lookup` lmap of 
--                                              Nothing -> []
--                                              Just bb -> [blockId bb]
--
--successor :: BasicBlock -> [[Int]] -> [BasicBlock] 
--successor bb m = undefined
--
--
--pprint blks = concatMap (\blk -> show blk ++ "\n") blks
--
--genGenSets :: [BasicBlock] -> [BasicBlock]
--genGenSets [] = []
--genGenSets (bb:bbs) = bb { genSet = g'} : genGenSets bbs
--  where
--    g' :: S.Set Instruction
--    g' = S.fromList [l | l <- unBasicBlock bb, opcode l `elem` defOpcodes]
--
---- populateKillSet assumes gen set has already been populated
--genKillSets :: M.Map Variable [Instruction] -> [BasicBlock] -> [BasicBlock]
--genKillSets _ [] = []
--genKillSets wmap blocks@(bb:bbx) = bb { killSet = S.fromList k'} : genKillSets wmap bbx
--  where
--    -- defInsts is all instructions that re-defines defitions in bb, including those in bb
--    defInsts = concatMap (\v -> maybe [] id (M.lookup v wmap)) (concatMap defVars (genSet bb))
--    -- k' excludes definitions in bb from defInsts
--    k' = filter (`S.notMember` genSet bb) defInsts
--
--genInOutSets :: [BasicBlock] -> [BasicBlock]
--genInOutSets [] = []
--genInOutSets blocks@(bb:bbx) = bb { inSet = i', outSet = o'} : genInOutSets bbx
--  where
--    i' :: S.Set Instruction
--    --i' = S.unions $ map outSet $ successor bb
--    i' = undefined 
--    o' :: S.Set Instruction
--    o' = (genSet bb) `S.union` (i' `S.difference` (killSet bb))
--
--
--
--
--



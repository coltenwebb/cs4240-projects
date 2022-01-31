module IR.Optimizer.ReachingDefs where

import IR.Instruction
import IR.Optimizer.MarkSweep

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')

-- so we need a way to represent the cfg
-- we will have a list of basic blocks
-- each basic block will be a list of instructions itself
--
-- the textbook gives pseudocode pg 241


instance Show BasicBlock where
  show bb =
    "\n" ++ show (blockId bb) ++ "\n" ++
    "    IN: " ++ show (S.map lineNum (inSet bb)) ++ "\n" ++
    "    OUT: " ++ show (S.map lineNum (outSet bb)) ++ "\n" ++
    "    GEN: " ++ show (S.map lineNum (genSet bb)) ++ "\n" ++
    "    KILL: " ++ show (S.map lineNum (killSet bb)) ++ "\n" ++
    "    insts[ " ++ show (map lineNum (unBasicBlock bb)) ++ "]\n"

insertEveryOther :: Show a => [Char] -> [a] -> [Char]
insertEveryOther _ [] = []
insertEveryOther i (x:xs) = show x ++ i ++ insertEveryOther i xs

isBranching :: Instruction -> Bool
isBranching inst
  | opcode inst `elem` [
      BREQ, BRNEQ, BRLT, BRGT, BRGEQ, BRLEQ, GOTO, RETURN
    ] = True
  | otherwise = False


createBasicBlocks :: [Instruction] -> [BasicBlock]
createBasicBlocks = g . foldl' f ([], [])
  where
    g = undefined
    
    f (currBlk, res) ins
      | null currBlk 
--createBasicBlocks insts = assignBlockId 0 (split insts)

split :: [Instruction] -> [BasicBlock]
split insts = foldl handle [] insts
  where
    handle :: [BasicBlock] -> Instruction -> [BasicBlock]
    handle blks inst
      | null blks = [createBB [inst]]
      | isBranching (lastInst blks) = blks ++ [createBB [inst]]
      | isLabel inst = blks ++ [createBB [inst]]
      | otherwise = init blks ++ [createBB (unBasicBlock (last blks) ++ [inst])]
    lastInst blks = last . unBasicBlock $ last blks
    createBB :: [Instruction] -> BasicBlock
    createBB insts = BasicBlock insts mempty mempty mempty mempty 0

assignBlockId :: Int -> [BasicBlock] -> [BasicBlock]
assignBlockId _ [] = []
assignBlockId i blocks@(bb:bbx) = bb {blockId = i} : assignBlockId (i+1) bbx

labelMap :: [BasicBlock] -> M.Map LabelName BasicBlock
labelMap blks = foldl handle M.empty blks
  where
    handle mp blk@(BasicBlock ((Instruction _ (LabelOperand lbn:_) _):_) _ _ _ _ _) = M.insert lbn blk mp
    handle mp _ = mp

neighbors :: M.Map BasicBlock [BasicBlock]
neighbors = undefined

-- row index represents src node, col index represents dest node
adjacencyMatrix :: [BasicBlock] -> [[Int]]
adjacencyMatrix blocks = map handle (zip [0..] blocks)
  where
    handle :: (Int, BasicBlock) -> [Int]
    handle (i, bb)
      | i == length blocks - 1      = replicate (length blocks) 0 
      | lastOp == RETURN            = replicate (length blocks) 0 
      | lastOp == GOTO              = [if x == head labelBlockId then 1 else 0 | x <- [0..length blocks - 1]]
      | lastOp `elem` branchOpcodes = [if x `elem` (i+1) : labelBlockId then 1 else 0 | x <- [0..length blocks - 1]] 
      | otherwise                   = [if x == i + 1 then 1 else 0 | x <- [0..length blocks - 1]]
      where
        lmap = labelMap blocks
        lastInst                      = last (unBasicBlock bb)
        lastOp                        = opcode lastInst
        getLabel (LabelOperand lname) = Just lname
        getLabel _                    = Nothing
        label                         = getLabel (head (operands lastInst))
        labelBlockId                  = case label of
                                          Nothing -> []
                                          Just l -> 
                                            case l `M.lookup` lmap of 
                                              Nothing -> []
                                              Just bb -> [blockId bb]

successor :: BasicBlock -> [[Int]] -> [BasicBlock] 
successor bb m = undefined


pprint blks = concatMap (\blk -> show blk ++ "\n") blks

genGenSets :: [BasicBlock] -> [BasicBlock]
genGenSets [] = []
genGenSets (bb:bbs) = bb { genSet = g'} : genGenSets bbs
  where
    g' :: S.Set Instruction
    g' = S.fromList [l | l <- unBasicBlock bb, opcode l `elem` defOpcodes]

-- populateKillSet assumes gen set has already been populated
genKillSets :: M.Map Variable [Instruction] -> [BasicBlock] -> [BasicBlock]
genKillSets _ [] = []
genKillSets wmap blocks@(bb:bbx) = bb { killSet = S.fromList k'} : genKillSets wmap bbx
  where
    -- defInsts is all instructions that re-defines defitions in bb, including those in bb
    defInsts = concatMap (\v -> maybe [] id (M.lookup v wmap)) (concatMap defVars (genSet bb))
    -- k' excludes definitions in bb from defInsts
    k' = filter (`S.notMember` genSet bb) defInsts

genInOutSets :: [BasicBlock] -> [BasicBlock]
genInOutSets [] = []
genInOutSets blocks@(bb:bbx) = bb { inSet = i', outSet = o'} : genInOutSets bbx
  where
    i' :: S.Set Instruction
    --i' = S.unions $ map outSet $ successor bb
    i' = undefined 
    o' :: S.Set Instruction
    o' = (genSet bb) `S.union` (i' `S.difference` (killSet bb))








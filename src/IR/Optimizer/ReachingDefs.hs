module IR.Optimizer.ReachingDefs where

import IR.Instruction
import qualified Data.Map as M

-- so we need a way to represent the cfg
-- we will have a list of basic blocks
-- each basic block will be a list of instructions itself
--
-- the textbook gives pseudocode pg 241

newtype BasicBlock = BasicBlock { unBasicBlock :: [Instruction] }
  deriving (Show)

isBranching inst
  | opcode inst `elem` [
      BREQ, BRNEQ, BRLT, BRGT, BRGEQ, BRLEQ, GOTO, RETURN
    ] = True
  | otherwise = False

isLabel inst = opcode inst == LABEL

split :: [Instruction] -> [BasicBlock]
split insts = foldl handle [] insts
  where
    handle :: [BasicBlock] -> Instruction -> [BasicBlock]
    handle blks inst
      | null blks = [BasicBlock [inst]]
      | isBranching (lastInst blks) = blks ++ [BasicBlock [inst]]
      | isLabel inst = blks ++ [BasicBlock [inst]]
      | otherwise = init blks ++ [BasicBlock ((unBasicBlock $ last blks) ++ [inst])]
    lastInst blks = last . unBasicBlock $ last blks

labelMap :: [BasicBlock] -> M.Map LabelName BasicBlock
labelMap blks = foldl handle M.empty blks
  where 
    handle mp blk@(BasicBlock ((Instruction _ (LabelOperand lbn:_) _):_)) = M.insert lbn blk mp
    handle mp _ = mp

neighbors :: M.Map BasicBlock [BasicBlock]
neighbors = undefined

pprint blks = concatMap (\blk -> show blk ++ "\n") blks

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module IR.Optimizer.MarkSweep where

import IR.Instruction
import IR.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (Foldable(toList))
import Debug.Trace

import Data.List
--import Data.Set

-- Map: Variable -> Instructions which write to said Variable
type WriteMap = M.Map Variable [Instruction]

isCritical :: Instruction -> Bool
isCritical inst
  | opcode inst `elem` [
      GOTO, BREQ, BRNEQ, BRLT, BRGT, 
      BRLEQ, BRGEQ, RETURN, CALL, CALLR,
      -- not certain whether array stuff should be critical
      -- array_store maybe since it may be accessed outside the function
      ARRAY_STORE
    ] = True
  | otherwise = False

genWriteMap :: Function -> WriteMap
genWriteMap f = M.fromListWith (++) allVarInstPairs
  where
  varInstPairs inst = map (\var->(var, [inst])) (defVars inst)
  allVarInstPairs = concatMap varInstPairs (instruction f)
-- M.fromList

-- TODO: Make sure to add labels in the optimization
simpleMarkSweep :: Function -> Function
simpleMarkSweep fn = buildFuncFromLineNumbers
  where
    wmap :: WriteMap
    wmap = genWriteMap fn

    criticals :: [Instruction]
    criticals = filter isCritical $ instruction fn

    bfs :: [Instruction] -> S.Set LineNumber -> S.Set LineNumber -> S.Set LineNumber
    bfs worklist marked visited
      | null worklist = marked
      | otherwise = bfs worklist' marked' visited'
        where
          worklist' = tail worklist ++ concatMap lookupWmap (usedVars (head worklist))
          visited' = lineNum (head worklist) `S.insert` visited

          lookupWmap :: Variable -> [Instruction]
          lookupWmap v = maybe [] filterVisited (M.lookup v wmap)

          filterVisited :: [Instruction] -> [Instruction]
          filterVisited ix = filter (\i -> not (lineNum i `S.member` visited)) ix

          marked' :: S.Set LineNumber
          marked' = lineNum (head worklist) `S.insert` marked

    buildFuncFromLineNumbers :: Function
    buildFuncFromLineNumbers = Function fnName fnRetType fnParams fnVars fnInstructions
      where
        optimizedInstructions :: [LineNumber]
        optimizedInstructions = toList $ bfs criticals mempty (S.fromList (map lineNum criticals))
        fnName = name fn
        fnRetType = returnType fn
        fnParams = parameters fn
        fnVars = variables fn
        fnInstructions = map (getInstructionByLineNum fn) optimizedInstructions
 

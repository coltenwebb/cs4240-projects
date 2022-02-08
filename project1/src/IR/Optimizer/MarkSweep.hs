{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module IR.Optimizer.MarkSweep where

import IR.Instruction
import IR.Function as F
import IR.Optimizer.ReachingDefs
import IR.Optimizer.CFG as C

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.Monoid

import Data.Foldable (Foldable(toList))
import qualified Data.Sequence as SE

-- import Debug.Trace

import Data.List
import Data.Maybe (catMaybes, fromMaybe)
--import Data.Set

isCritical :: Instruction -> Bool
isCritical inst
  | opcode inst `elem` [
      GOTO, BREQ, BRNEQ, BRLT, BRGT,
      BRLEQ, BRGEQ, RETURN, CALL, CALLR,
      LABEL,
      -- not certain whether array stuff should be critical
      -- array_store maybe since it may be accessed outside the function
      ARRAY_STORE
    ] = True
  | otherwise = False

markSweepSimpleAndReachDef :: Function -> Function
markSweepSimpleAndReachDef = markSweepWithReachDef . simpleMarkSweep

-- Map: Variable -> Instructions which write to said Variable
markSweepWithReachDef :: Function -> Function
markSweepWithReachDef fn = fn { F.instrs = optimizedInstrs }
  where
    cfg = makeCFG . F.instrs $ fn
    bbs = getBasicBlocks cfg
    ReachDefResult inSets _ _ _ = runReachingDefAlgorithm cfg

    optimizedInstrs :: [Instruction]
    optimizedInstrs =
      let marked = bfs (SE.fromList criticals) (S.fromList (lineNum <$> criticals))
      in filter ((`S.member` marked) . lineNum) (F.instrs fn)

    criticals :: [Instruction]
    criticals = [ins | bb <- bbs, ins <- NE.toList (C.instrs bb), isCritical ins]

    lineNumberLookup :: LineNumber -> BasicBlock
    lineNumberLookup ln = fromMaybe (error "unpossible") (M.lookup ln t)
      where
        t :: M.Map LineNumber BasicBlock
        t = M.fromList [(lineNum ins, bb) | bb <- bbs, ins <- NE.toList (C.instrs bb)]

    lineNumberInsLookup :: LineNumber -> Instruction
    lineNumberInsLookup ln = fromMaybe (error "unpossible") (M.lookup ln t)
      where
        t :: M.Map LineNumber Instruction
        t = M.fromList [(lineNum ins, ins) | bb <- bbs, ins <- NE.toList (C.instrs bb)]

    bfs :: SE.Seq Instruction -> S.Set LineNumber -> S.Set LineNumber
    bfs worklist marked = case SE.viewl worklist of
      SE.EmptyL -> marked
      currIns SE.:< wl ->
        let
          uses :: [Variable]
          uses = usedVars currIns

          currBlkId = blockId . lineNumberLookup . lineNum $ currIns

          currBlock :: BasicBlock
          currBlock = fromMaybe (error "Unexpected BlockId not found") $
            currBlkId `M.lookup` blockLookup cfg

          -- Shouldn't be Nothing, because we should have initialized this
          -- for every single BlockId
          currInSet :: InSet
          currInSet = fromMaybe (InSet mempty) (currBlkId `M.lookup` inSets)

          -- Lecture 4, pg. 4
          --  A def reaches instruction i
          -- 1) if it is in the IN set for the basic block B(i) containing i, and
          -- 2) the def is not killed locally within B(i) before instruction i
          -- • This algorithm assumes that branch instructions are treated as
          -- critical instructions

          -- Each var may have multiple defs that reach the uses in currIns
          -- Reaches from outside the block (in set)
          interBlockReaches :: M.Map Variable (S.Set Instruction)
          interBlockReaches = M.fromListWith mappend
            [(v, S.singleton i) | i <- S.toList (unInSet currInSet),
                                  v <- defVars i,
                                  v `elem` uses]

          -- Condition 2 + reaching defs in the same block, we replace
          -- an existing potential reaching def with the def that killed it
          reachingDefs :: M.Map Variable (S.Set Instruction)
          reachingDefs = foldl' f interBlockReaches b
            where
              b = takeWhile (/= currIns) (NE.toList (C.instrs currBlock))
              f p instr =
                let dv = defVars instr
                -- `M.union` is left-biased, prefers first arg. in conflict
                in (M.fromList [(v, S.singleton instr) | v <- dv, v `elem` uses]) `M.union` p

          lns = lineNum <$> (M.elems reachingDefs >>= S.toList)
          unmarked = filter (not . (`S.member` marked)) lns
          unmarkedIns = map lineNumberInsLookup unmarked

          worklist' = wl SE.>< SE.fromList unmarkedIns
          marked' = marked <> S.fromList (lineNum <$> unmarkedIns)

    --      in trace ("!: " ++ show (lineNum currIns) ++ (show (lineNum <$> unmarkedIns))) $ bfs worklist' marked'
          in bfs worklist' marked'

-- TODO: Make sure to add labels in the optimization
simpleMarkSweep :: Function -> Function
simpleMarkSweep fn = buildFuncFromLineNumbers
  where
    wmap :: WriteMap
    wmap = genWriteMap (F.instrs fn)

    criticals :: [Instruction]
    criticals = filter isCritical $ F.instrs fn

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


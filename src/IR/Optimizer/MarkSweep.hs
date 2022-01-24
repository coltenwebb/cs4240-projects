module IR.Optimizer.MarkSweep where

import IR.Instruction
import IR.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (Foldable(toList))

-- Map: Variable -> Instructions which write to said Variable
type WriteMap = M.Map Variable Instruction

isCritical :: Instruction -> Bool
isCritical = undefined

genWriteMap :: Function -> WriteMap
genWriteMap = undefined
-- M.fromList

uses :: Instruction -> [Variable]
uses = undefined

-- TODO: Test the heck out of it
simpleMarkSweep :: Function -> Function
simpleMarkSweep fn = buildFuncFromLineNumbers
  where
    wmap :: WriteMap
    wmap = genWriteMap fn

    criticals :: Function -> [Instruction]
    criticals = undefined -- TODO: find all side-effect functions

    bfs :: [Instruction] -> S.Set LineNumber -> S.Set LineNumber
    bfs worklist marked
      | null worklist = marked
      | otherwise = bfs worklist' marked'
        where
          worklist' = tail worklist
                        ++ map lookupWmap (uses (head worklist))

          lookupWmap :: Variable -> Instruction
          lookupWmap v = case M.lookup v wmap of
                          Nothing -> Instruction ASSIGN [] (LineNumber 1)
                          Just i -> i

          marked' :: S.Set LineNumber
          marked' = lineNum (head worklist) `S.insert` marked
    
    buildFuncFromLineNumbers :: Function
    buildFuncFromLineNumbers = Function fnName fnRetType fnParams fnVars fnInstructions
      where 
        lineNumbers = toList $ bfs (criticals fn) mempty 
        fnName = name fn 
        fnRetType = returnType fn
        fnParams = parameters fn 
        fnVars = variables fn 
        fnInstructions = map indexLinenum $ toList lineNumbers
          where indexLinenum (LineNumber x) = instruction fn !! x



-- simpleMarkSweep fn = filter instruction by marked
--   where
--     wmap :: WriteMap
--     wmap = genWriteMap fn

--     bfs :: Queue -> M.Set LineNumber -> M.Set LineNumber
--     bfs worklist marked
--         | null worklist = marked
--         | otherwise = take 1 variable v from worklist,
--             look into genWriteMap, for each instr in writemap[v]
--             we add instruction into workqueue.
--             bfs ((drop 1 worklist) ++ wmap[v]) (marked ++ line number of v)

-- 1. get inst from fn
-- 2. find all side Effect inst (isCritical) and add to worklist
-- 3. for i in worklist, if i is output sideeffect inst, look up var in wmap, add inst to worklist
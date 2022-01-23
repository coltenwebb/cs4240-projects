module IR.Optimizer.MarkSweep where

import IR.Instruction
import IR.Function

import qualified Data.Map as M

-- Map: Variable -> Instructions which write to said Variable
type WriteMap = M.Map Variable Instruction

isCritical :: Instruction -> Bool
isCritical = undefined

genWriteMap :: Function -> WriteMap
genWriteMap = undefined
-- M.fromList

simpleMarkSweep :: Function -> Function
simpleMarkSweep = undefined
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
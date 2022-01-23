module IR.Optimizer.MarkSweep where

import IR.Instruction
import IR.Function

import qualified Data.Map as M

-- Map: Variable -> Instructions which write to said Variable
type WriteMap = M.Map Variable Instruction

isCritical :: Instruction -> Bool
isCritical Instruction {opcode=GOTO} = True
isCritical Instruction {opcode=BREQ} = True
isCritical Instruction {opcode=BRNEQ} = True
isCritical Instruction {opcode=BRLT} = True
isCritical Instruction {opcode=BRGT} = True
isCritical Instruction {opcode=BRLEQ} = True
isCritical Instruction {opcode=BRGEQ} = True
isCritical Instruction {opcode=RETURN} = True
isCritical Instruction {opcode=CALL} = True
isCritical Instruction {opcode=CALLR} = True

-- not certain whether array stuff should be critical
-- array_store maybe since it may be accessed outside the function
isCritical Instruction {opcode=ARRAY_STORE} = True
--isCritical Instruction {opcode=ARRAY_LOAD} = True

isCritical _ = False

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

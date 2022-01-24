module IR.Optimizer.MarkSweep where

import IR.Instruction
import IR.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (Foldable(toList))

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

-- TODO: Test it
simpleMarkSweep :: Function -> Function
simpleMarkSweep fn = buildFuncFromLineNumbers
  where
    wmap :: WriteMap
    wmap = genWriteMap fn

    criticals :: [Instruction]
    criticals = filter isCritical $ instruction fn

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
        lineNumbers = toList $ bfs criticals mempty 
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

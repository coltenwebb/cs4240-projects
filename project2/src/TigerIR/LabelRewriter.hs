module TigerIR.LabelRewriter where

import TigerIR.IrInstruction
import TigerIR.Program
import TigerIR.Types

labelRewriteInstrPass :: TigerIrFunction -> TigerIrIns -> TigerIrIns
labelRewriteInstrPass fn (Instruction ins ln) = Instruction ins' ln
  where
    (FunctionName (Label fnName)) = name fn
    prependFnName (Label label) = Label (fnName ++ "_" ++ label)

    ins' = case ins of
      LabelIns label -> LabelIns (prependFnName label)
      BranchOperation brop label oprnds
        -> BranchOperation brop (prependFnName label) oprnds
      Goto label -> Goto (prependFnName label)
      x -> x

labelRewriteFnPass :: TigerIrFunction -> TigerIrFunction
labelRewriteFnPass tigerFn = tigerFn { instrs = instrs' }
  where
    instrs' = map (labelRewriteInstrPass tigerFn) (instrs tigerFn)

labelRewriteProgPass :: TigerIrProgram -> TigerIrProgram
labelRewriteProgPass (Program fns) = Program fns'
  where
    fns' = map labelRewriteFnPass fns
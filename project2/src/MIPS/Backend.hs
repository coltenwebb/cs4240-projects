module MIPS.Backend where

import MIPS.Types.Physical (PhysicalProgram)
import TigerIR.Program
import TigerIR.LabelRewriter (labelRewriteFnPass)
import MIPS.Selection (virtFnSelection)
import qualified MIPS.RegisterAllocator.Naive  as N (physFnSelection)
import qualified MIPS.RegisterAllocator.Greedy as G (physFnSelection)

naiveProgramSelection :: TigerIrProgram -> PhysicalProgram
naiveProgramSelection = programSelectionPass f
  where
    f = N.physFnSelection . virtFnSelection . labelRewriteFnPass

greedyProgramSelection :: TigerIrProgram -> PhysicalProgram
greedyProgramSelection = programSelectionPass f
  where
    f = G.physFnSelection . virtFnSelection . labelRewriteFnPass
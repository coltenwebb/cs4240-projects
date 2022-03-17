module MIPS.Backend where

import MIPS.Types.Physical (PhysicalProgram)
import TigerIR.Program
import MIPS.Selection (virtFnSelection)
import qualified MIPS.RegisterAllocator.Naive as N (physFnSelection)

naiveProgramSelection :: TigerIrProgram -> PhysicalProgram
naiveProgramSelection = programSelectionPass f
  where
    f = N.physFnSelection . virtFnSelection
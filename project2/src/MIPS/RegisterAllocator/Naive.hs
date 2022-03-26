module MIPS.RegisterAllocator.Naive where

import MIPS.RegisterAllocator.Monad.NaiveM
import MIPS.RegisterAllocator.Allocation
import MIPS.CallingConvention
import TigerIR.Program
import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P

physFnSelection :: V.VirtualFunction -> P.PhysicalFunction
physFnSelection vf = vf { instrs = pinsts }
  where
    regmap = calcRegMap vf
    vinsts = instrs vf
    naiveM = mapM_ (virtToEmitPhysMIPS vf) vinsts
    pinsts = runNaiveM naiveM regmap


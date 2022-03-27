module MIPS.RegisterAllocator.Greedy where
import qualified MIPS.Types.Physical as P

physFnSelection :: V.VirtualFunction -> P.PhysicalFunction
physFnSelection vf = 
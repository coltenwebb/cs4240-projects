module MIPS.RegisterAllocator.Greedy where

import MIPS.RegisterAllocator.Monad.GreedyM
import MIPS.RegisterAllocator.Allocation
import MIPS.RegisterAllocator.Greedy.MipsCFG as BB
import MIPS.RegisterAllocator.Greedy.Graph
import MIPS.CallingConvention
import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P
import TigerIR.Program as T

import Debug.Trace

physFnSelection :: V.VirtualFunction -> P.PhysicalFunction
physFnSelection vf = vf { T.instrs = pinsts }
  where
    regmap = calcRegMap vf

    vinstrs :: [V.MipsVirtual]
    vinstrs = T.instrs vf

    virtBblks :: [BasicBlockMips]
    virtBblks = let k = splitIntoBasicBlocks vinstrs
                in traceShow k k

    bbSel :: BasicBlockMips -> [P.MipsPhys]
    bbSel = basicBlockSelection vf regmap

    pinsts = concatMap bbSel virtBblks

basicBlockSelection
  :: V.VirtualFunction
  -> RegMap
  -> BasicBlockMips
  -> [P.MipsPhys]
basicBlockSelection vf rm bbm = pinsts
  where
    bbIns       = BB.instrs bbm
    colorLookup = runGreedyColoring bbm
    greedyM     = mapM_
      (\x -> virtToEmitPhysMIPS vf x >> incrBBL) bbIns
    pinsts      = runGreedyM greedyM rm colorLookup
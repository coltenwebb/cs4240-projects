module MIPS.RegisterAllocator.Greedy where

import MIPS.RegisterAllocator.Monad.GreedyM
import MIPS.RegisterAllocator.Allocation
import MIPS.RegisterAllocator.Greedy.MipsCFG as BB
import MIPS.RegisterAllocator.Greedy.Graph
import MIPS.CallingConvention
import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P
import TigerIR.Program as T

import Control.Monad.State

physFnSelection :: V.VirtualFunction -> P.PhysicalFunction
physFnSelection vf = vf { T.instrs = pinsts }
  where
    regmap = calcRegMap vf

    vinstrs :: [V.MipsVirtual]
    vinstrs = T.instrs vf

    virtBblks :: [BasicBlockMips]
    virtBblks = splitIntoBasicBlocks vinstrs

    bbSel :: BasicBlockMips -> State UniqueCounter [P.MipsPhys]
    bbSel = basicBlockSelection vf regmap

    pinsts = concat $ evalState (mapM bbSel virtBblks) (U 0)

basicBlockSelection
  :: V.VirtualFunction
  -> RegMap
  -> BasicBlockMips
  -> State UniqueCounter [P.MipsPhys]
basicBlockSelection vf rm bbm = do
  uc <- get
  let (uc', pinsts) = runGreedyM uc greedyM rm colorLookup
  put uc'
  return pinsts
  where
    bbIns       = BB.instrs bbm
    colorLookup = runGreedyColoring bbm
    greedyM     = mapM_
      (\x -> virtToEmitPhysMIPS vf x >> incrBBL) bbIns


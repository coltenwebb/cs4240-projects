module MIPS.Liveness.InstCFG where
import MIPS.Types.Physical (MipsPhys)
import qualified Data.Map as M
import qualified Data.Set as S
import MIPS.Types.Virtual (MipsVirtual)
import IR.CFG.BasicBlockGeneral

data InstBlock = InstBlock
  { instr :: MipsPhys
  , blockId :: InstId
  } deriving Show 

type BasicBlockMips = BasicBlockGeneral MipsVirtual

newtype InstId = InstId Int
  deriving (Eq, Ord, Show)


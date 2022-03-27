module MIPS.RegisterAllocator.Greedy.MipsCFG where

import MIPS.Types.Physical (MipsPhys)
import qualified Data.Map as M
import qualified Data.Set as S
import MIPS.Types.Virtual as V
import Data.List.NonEmpty as NE hiding (map)
import Data.List as L (foldl', reverse, head, intercalate)

class Mips a where
  isBranching :: a -> Bool
  isLabel     :: a -> Bool
  isReturnOp  :: a -> Bool
  isGotoOp    :: a -> Bool

instance Mips MipsVirtual where
  isBranching BrVI {}     = True
  isBranching BrVV {}     = True
  isBranching BrIV {}     = True
  isBranching BrII {}     = True
  isBranching _           =  False 

  isLabel V.LabelIns {}   = True 
  isLabel _               = False 

  isReturnOp V.Return {}  = True 
  isReturnOp V.Returni {} = True 
  isReturnOp _            = False 

  isGotoOp V.Goto {}      = True 
  isGotoOp _              = False


data BasicBlockGeneral a = BasicBlock
  { instrs :: NonEmpty a
  , lastIns :: a
  , blockId :: BlockId
  }

type BasicBlockMips = BasicBlockGeneral MipsVirtual

newtype InstId = InstId Int
  deriving (Eq, Ord, Show)

newtype BlockId = BlockId Int
  deriving (Eq, Ord, Show)

instance (Show a) => Show (BasicBlockGeneral a) where 
  show (BasicBlock insts lastInst (BlockId bid)) = "\n" ++ show bid ++ ": {  " ++ 
    intercalate "\n     , " (map show (NE.head insts : NE.tail insts)) ++ " }"


splitIntoBasicBlocks :: (Mips a) => [a] -> [BasicBlockGeneral a]
splitIntoBasicBlocks [] = []
splitIntoBasicBlocks (x:xs) = g . foldl f (x :| [] , [], 0) $ xs
  where
    -- Prepend for O(1) time
    appendIns currBlk ins = ins <| currBlk

    -- Reverse because we prepended
    toBlk blk i = BasicBlock (NE.reverse blk) (NE.head blk) (BlockId i)

    prevIns = NE.head

    -- Need to reverse because BasicBlock result list was also prepended
    g (lastBlk, res, cnt) = L.reverse $ toBlk lastBlk cnt : res

    -- We also process the prevIns in order to use NonEmpty list logic
    f (currBlk, res, cnt) currIns
      | isBranching (prevIns currBlk) = (currIns :| [], res', cnt+1)
      | isLabel currIns               = (currIns :| [], res', cnt+1)
      | otherwise                     = (currBlk `appendIns` currIns, res, cnt)
      where
        curr = prevIns currBlk
        res' = toBlk currBlk cnt : res

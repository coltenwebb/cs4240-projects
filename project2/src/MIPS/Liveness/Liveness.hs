module MIPS.Liveness.Liveness where 

import TigerIR.IrInstruction
import TigerIR.Types
import MIPS.Types.Operand
import Data.Maybe
import Debug.Trace
import Data.List

import Control.Monad.State.Lazy

import MIPS.Types.Virtual

-- a live range consists of a virtual register, start instruction, end instruction, use count (def count will always be 1)
-- we generate a set of live ranges

-- then allocation will insert loads before some live ranges and stores after.


-- for testing purposes

testInst :: [MipsVirtualLine]
testInst = [
    MipsVirtualLine (AssignI (VReg (Variable "x")) (Imm "1")) 0,
    MipsVirtualLine (AssignV (VReg (Variable "y")) (VReg (Variable "x"))) 1
  ]
--testInst = [
--    IrInstruction (AssignVar (AssignVarOpsVI (Variable "x") (Imm "1"))) (LineNumber 1) ,
--    IrInstruction (AssignVar (AssignVarOpsVI (Variable "y") (Imm "2"))) (LineNumber 2) 
--  ]

defVar :: MipsVirtual -> Maybe VReg
defVar (AssignI vreg _) = Just vreg
defVar (AssignV vreg _) = Just vreg
--defVar (AssignVar (AssignVarOpsVV var _)) = Just var
--defVar (AssignVar (AssignVarOpsVI var _)) = Just var

usedVars :: MipsVirtual -> [VReg]
usedVars (AssignI _ _) = []
usedVars (AssignV _ vreg) = [vreg]
--usedVars (AssignVar (AssignVarOpsVV _ var)) = [var]
--usedVars (AssignVar (AssignVarOpsVI _ _)) = []

type Line = Integer
data MipsVirtualLine = MipsVirtualLine MipsVirtual Line deriving Show

data LiveRange = LiveRange { unVReg :: VReg, unStart :: Line, unEnd :: Line, unUses :: Integer, unNeedsStore :: Bool } deriving Show
data PartialLiveRange = PartialLiveRange { unPVReg :: VReg, unPEnd :: Line, unPUses :: Integer } deriving Show

type BuildLiveRangesState = ([PartialLiveRange], [LiveRange])

isVRegLive plrs vreg = any (\plr -> vreg == unPVReg plr) plrs

buildLiveRanges :: [MipsVirtualLine] -> [LiveRange]
buildLiveRanges vinsts = finish $ foldr f ([],[]) vinsts
  where
    f :: MipsVirtualLine -> BuildLiveRangesState -> ([PartialLiveRange], [LiveRange])
    f vinst = (use vinst) . (kill vinst)
    kill :: MipsVirtualLine -> BuildLiveRangesState -> BuildLiveRangesState
    kill (MipsVirtualLine vinst line) st@(plrs, lrs) = case defVar vinst of
      Nothing -> st
      Just vreg -> case findKill vreg of
        Nothing -> insertUnreadLiveRange vreg
        Just plr -> insertReadLiveRange plr vreg
      where
        findKill vreg                = find (\plr -> Just (unPVReg plr) == defVar vinst) plrs
        insertUnreadLiveRange vreg  = (plrs, (LiveRange vreg line line 0 (needsStore vreg)) : lrs)
        insertReadLiveRange plr vreg = (filter (((/=) vreg) . unPVReg) plrs, ((LiveRange (unPVReg plr) line (unPEnd plr) (unPUses plr) (needsStore vreg)):lrs))
        needsStore vreg = not $ any (\lr -> unVReg lr == vreg) lrs
    use :: MipsVirtualLine -> BuildLiveRangesState -> BuildLiveRangesState
    use (MipsVirtualLine vinst line) st = foldl' doUse st (usedVars vinst)
      where
        doUse :: BuildLiveRangesState -> VReg -> BuildLiveRangesState
        doUse st@(plrs, lrs) vreg
          | isVRegLive plrs vreg = usePartialLiveRange st vreg
          | otherwise            = insertPartialLiveRange st vreg
        insertPartialLiveRange (plrs, lrs) vreg = ((PartialLiveRange vreg line 1 : plrs), lrs)
        usePartialLiveRange (plrs, lrs) vreg
          = ([if (vreg == unPVReg plr) then (incPartialLiveRangeUses plr) else plr | plr <- plrs], lrs)
        incPartialLiveRangeUses (PartialLiveRange vreg end uses) = PartialLiveRange vreg end (uses + 1)
    finish :: BuildLiveRangesState -> [LiveRange]
    finish (plrs, lrs) = lrs ++ [(LiveRange vreg (findFirstUse vreg) end uses False) | (PartialLiveRange vreg end uses) <- plrs]
      where
        findFirstUse :: VReg -> Line
        findFirstUse vreg = case find (\(MipsVirtualLine vinst line) -> vreg `elem` usedVars vinst) vinsts of
          (Just (MipsVirtualLine vinst line)) -> line
          Nothing -> error "somehow a partial live range exists but the variable isn't used..."
        
            





--data LiveRange = LiveRange { variable :: Variable, start :: LineNumber, end :: LineNumber } deriving (Show)
--type LiveRangesState = ([LiveRange])
--
--go :: [IrInstruction] -> State LiveRangesState ([LiveRange])
--go (inst:rem) = do
--  liveRanges <- get
--  -- if its a read and no live range exists for the variable, create a new live range
--  go rem
--go [] = do
--  x <- get
--  return x
--  
--test = print $ evalState (go $ reverse testInst) []
--
--



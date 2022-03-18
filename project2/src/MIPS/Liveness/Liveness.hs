{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module MIPS.Liveness.Liveness where 

import qualified TigerIR.IrInstruction as T
import qualified TigerIR.Types as T
import MIPS.Types.Operand
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.State.Lazy

import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P

import MIPS.CallingConvention
import TigerIR.Program

-- a live range consists of a virtual register, start instruction, end instruction, use count (def count will always be 1)
-- we generate a set of live ranges

-- then allocation will insert loads before some live ranges and stores after.


-- for testing purposes

testInst :: [MVLine]
testInst = [
    MVLine (V.Add (VReg (Variable "d")) (VReg (Variable "d")) (VReg (Variable "d"))) 1
  ]

tfunc :: Function V.MipsVirtual
tfunc = Function
  { name = FunctionName (Label "tFunc")
  , returnsVal = False
  , parameters =
    [ ParamV (Variable "x")
    ]
  , localVars =
    [ LocalV (Variable "a")
    , LocalV (Variable "b")
    , LocalV (Variable "c")
    , LocalV (Variable "d")
    ]
  , instrs =
    [ V.AssignV (VReg (Variable "d")) (VReg (Variable "a"))
    , V.Mult (VReg (Variable "d")) (VReg (Variable "d")) (VReg (Variable "x"))
    , V.Add (VReg (Variable "d")) (VReg (Variable "d")) (VReg (Variable "b"))
    , V.Mult (VReg (Variable "d")) (VReg (Variable "d")) (VReg (Variable "x"))
    , V.Add (VReg (Variable "d")) (VReg (Variable "d")) (VReg (Variable "c"))
    ]
  }
-- tfunc generates the below instructions
-- [Lw (T T0) -8 Fp,
-- Addi (T T1) (T T0) 0,
-- Lw (T T0) -4 Fp,
-- Mult (T T1) (T T0),
-- Mflo (T T3),
-- Lw (T T1) -12 Fp,
-- Add (T T2) (T T3) (T T1),
-- Mult (T T2) (T T0),
-- Mflo (T T1),
-- Lw (T T0) -16 Fp,
-- Add (T T2) (T T1) (T T0),
-- Sw (T T2) -20 Fp]


--testInst = [
--    --MVLine (Add (VReg (Variable "a")) (VReg (Variable "d")) (VReg (Variable "d"))) 1,
--    MVLine (V.Add (VReg (Variable "a")) (VReg (Variable "c")) (VReg (Variable "d"))) 1,
--    MVLine (V.Add (VReg (Variable "e")) (VReg (Variable "c")) (VReg (Variable "b"))) 2,
--    MVLine (V.Add (VReg (Variable "f")) (VReg (Variable "e")) (VReg (Variable "c"))) 3,
--    MVLine (V.Add (VReg (Variable "g")) (VReg (Variable "e")) (VReg (Variable "f"))) 4,
--    MVLine (V.Add (VReg (Variable "e")) (VReg (Variable "c")) (VReg (Variable "a"))) 5 
--  ]

-- used to generate live ranges
defVar :: V.MipsVirtual -> Maybe VReg
defVar vinst = case vinst of
  V.AssignI vreg _    -> Just vreg
  V.AssignV vreg _    -> Just vreg
  V.Addi vreg _ _     -> Just vreg
  V.Li vreg _         -> Just vreg
  V.Add vreg _ _      -> Just vreg
  V.Sub vreg _ _      -> Just vreg
  V.SubVI vreg _ _      -> Just vreg
  V.SubIV vreg _ _      -> Just vreg
  V.Mult vreg _ _     -> Just vreg 
  V.Multi vreg _ _     -> Just vreg 
  V.Div vreg _ _      -> Just vreg 
  V.DivVI vreg _ _      -> Just vreg 
  V.DivIV vreg _ _      -> Just vreg 
  V.Andi vreg _ _     -> Just vreg 
  V.And vreg _ _      -> Just vreg  
  V.Ori vreg _ _      -> Just vreg  
  V.Or vreg _ _       -> Just vreg 
  V.Bri _ _ _ _       -> Nothing
  V.Br _ _ _ _        -> Nothing
  V.Lw vreg _ _       -> Just vreg
  V.Sw _ _ _          -> Nothing
  V.Label _             -> Nothing
  V.Goto _            -> Nothing
  V.Call _ _          -> Nothing 
  V.Callr vreg _ _    -> Just vreg
  V.ArrStr _ _ _      -> Nothing
  V.ArrStri _ _ _     -> Nothing
  V.ArrStrii _ _ _     -> Nothing
  V.ArrStriv _ _ _     -> Nothing
  V.ArrLoad vreg _ _  -> Just vreg
  V.ArrLoadi vreg _ _ -> Just vreg
  V.ArrAssignVV _ _ _ -> Nothing
  V.ArrAssignVI _ _ _ -> Nothing
  V.ArrAssignII _ _ _ -> Nothing
  V.ArrAssignIV _ _ _ -> Nothing
  V.Nop               -> Nothing
  V.Return _          -> Nothing
  V.Returni _         -> Nothing 
  V.BeginFunction         -> Nothing 
  V.EndFunction         -> Nothing 

-- used to generate live ranges
usedVars :: V.MipsVirtual -> [VReg]
usedVars vinst = case vinst of
  V.AssignI _ _              -> []
  V.AssignV _ vreg           -> [vreg]
  V.Addi _ vreg _            -> [vreg]
  V.Li _ _                   -> []
  V.Add _ vreg1 vreg2        -> [vreg1, vreg2]
  V.Sub _ vreg1 vreg2        -> [vreg1, vreg2]
  V.SubVI _ vreg _      -> [vreg]
  V.SubIV _ _ vreg      -> [vreg]
  V.Mult _ vreg1 vreg2       -> [vreg1, vreg2] 
  V.Multi _ vreg _       -> [vreg] 
  V.Div _ vreg1 vreg2        -> [vreg1, vreg2] 
  V.DivVI _ vreg _        -> [vreg] 
  V.DivIV _ _ vreg        -> [vreg] 
  V.Andi _ vreg1 _           -> [vreg1] 
  V.And _ vreg1 vreg2        -> [vreg1, vreg2]  
  V.Ori _ vreg1 _            -> [vreg1]  
  V.Or _ vreg1 vreg2         -> [vreg1, vreg2] 
  V.Bri _ vreg _ _           -> [vreg]
  V.Br _ vreg1 vreg2 _       -> [vreg1, vreg2]
  V.Lw _ _ _                 -> []
  V.Sw _ _ _                 -> []
  V.Label _                    -> []
  V.Goto _                   -> []
  V.Call _ args              -> mapMaybe vRegArg args
  V.Callr _ _ args           -> mapMaybe vRegArg args
  V.ArrStr vreg1 vreg2 vreg3 -> [vreg1, vreg2, vreg3]
  V.ArrStri vreg1 vreg2 _    -> [vreg1, vreg2]
  V.ArrStrii _ vreg _    -> [vreg]
  V.ArrStriv _ vreg1 vreg2    -> [vreg1, vreg2]
  V.ArrLoad _ vreg1 vreg2    -> [vreg1, vreg2]
  V.ArrLoadi _ vreg _        -> [vreg]
  V.ArrAssignVV vr1 vr2 vr3  -> [vr1, vr2, vr3]
  V.ArrAssignVI vr1 vr2 _    -> [vr1, vr2]
  V.ArrAssignII vreg _ _     -> [vreg]
  V.ArrAssignIV vr1 _ vr2    -> [vr1, vr2]
  V.Nop                      -> []
  V.Return vreg              -> [vreg]
  V.Returni _                -> []
  V.BeginFunction                -> []
  V.EndFunction                -> []
  where vRegArg arg = case arg of
          V.CVarg vreg -> Just vreg
          V.CIarg _    -> Nothing

firstUsed :: V.MipsVirtual -> Maybe VReg
firstUsed mv
  | length (usedVars mv) == 0 = Nothing
  | otherwise = Just $ head $ usedVars mv

secondUsed :: V.MipsVirtual -> Maybe VReg
secondUsed mv
  | length (usedVars mv) <= 1 = Nothing
  | otherwise = Just $ head . tail $ usedVars mv

type Line = Integer
data MVLine = MVLine V.MipsVirtual Line deriving Show

lined :: [V.MipsVirtual] -> [MVLine]
lined vins = zipWith MVLine vins [1..]

data LiveRange = LiveRange 
  { unVReg :: VReg
  , unStart :: Line
  , unEnd :: Line
  , unUses :: Integer
  , unNeedsStore :: Bool 
  -- cover the degenerate cases, will need them later
  , unHasDef :: Bool 
  } deriving Show
data PartialLiveRange = PartialLiveRange { unPVReg :: VReg, unPEnd :: Line, unPUses :: Integer } deriving Show

type BuildLiveRangesState = ([PartialLiveRange], [LiveRange])

isVRegLive plrs vreg = any (\plr -> vreg == unPVReg plr) plrs

buildLiveRanges :: [MVLine] -> [LiveRange]
buildLiveRanges vinsts = finish $ foldr f ([],[]) vinsts
  where
    f :: MVLine -> BuildLiveRangesState -> ([PartialLiveRange], [LiveRange])
    f vinst = (use vinst) . (kill vinst)
    kill :: MVLine -> BuildLiveRangesState -> BuildLiveRangesState
    kill (MVLine vinst line) st@(plrs, lrs) = case defVar vinst of
      Nothing -> st
      Just vreg -> case findPartialLiveRange vreg of
        Nothing -> insertUnusedLiveRange vreg
        Just plr -> insertUsedLiveRange plr vreg
      where
        findPartialLiveRange vreg = find (\plr -> Just (unPVReg plr) == defVar vinst) plrs
        insertUnusedLiveRange vreg  = (plrs, (LiveRange vreg line line 0 (needsStore vreg) True) : lrs)
        insertUsedLiveRange plr vreg = (filter (((/=) vreg) . unPVReg) plrs, ((LiveRange (unPVReg plr) line (unPEnd plr) (unPUses plr) (needsStore vreg) True):lrs))
        needsStore vreg = not $ any (\lr -> unVReg lr == vreg) lrs
    use :: MVLine -> BuildLiveRangesState -> BuildLiveRangesState
    -- make sure works with x = y+y
    use (MVLine vinst line) st = foldl' doUse st (nub $ usedVars vinst)
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
    finish (plrs, lrs) = lrs ++ [(LiveRange vreg (findFirstUse vreg) end uses False False) | (PartialLiveRange vreg end uses) <- plrs]
      where
        findFirstUse :: VReg -> Line
        findFirstUse vreg = case find (\(MVLine vinst line) -> vreg `elem` usedVars vinst) vinsts of
          (Just (MVLine vinst line)) -> line
          Nothing -> error "somehow a partial live range exists but the variable isn't used..."

-- Allocated Live Range
data ALR = ALR { unATmpReg :: Maybe TmpReg, unALR :: LiveRange } deriving Show



allocatePReg :: LiveRange -> [ALR] -> Maybe TmpReg
allocatePReg lr alrs 
  = listToMaybe $ [T0 .. T7] \\ mapMaybe unATmpReg (conflictingALRs lr alrs) 
conflictingALRs lr = filter (isConflicting lr) 
isConflicting lr (ALR _ olr) = not $ unStart lr > unEnd olr || unEnd lr < unStart olr

allocatePRegs :: [LiveRange] -> [ALR]
allocatePRegs lrs = foldl' f [] (reverse $ sortOn unUses lrs)
  where
    f :: [ALR] -> LiveRange -> [ALR]
    f alrs lr = (ALR (allocatePReg lr alrs) lr : alrs)


-- data HybridInstruction
--   = HiVirt MVLine
--   | HIPhys P.MipsPhys
-- 
-- toHybridInstructions :: [MVLine] -> [HybridInstruction]
-- toHybridInstructions = map HiVirt
-- 
-- insertPred :: (a -> Bool) -> a -> [a] -> [a]
-- insertPred pred el lst = let (x,y) = span pred lst in x ++ [el] ++ y
-- 
-- insertBefore :: Line -> P.MipsPhys -> [HybridInstruction] -> [HybridInstruction]
-- insertBefore ln inst hybrid = insertPred above (HIPhys inst) hybrid
--   where
--     above (HIPhys _) = True
--     above (HiVirt (MVLine _ hln)) = hln < ln
-- 
-- insertAfter :: Line -> P.MipsPhys -> [HybridInstruction] -> [HybridInstruction]
-- insertAfter ln inst hybrid = insertPred above (HIPhys inst) hybrid
--   where
--     above (HIPhys _) = True
--     above (HiVirt (MVLine _ hln)) = hln <= ln
-- 
-- indexByLine :: [HybridInstruction] -> Line -> V.MipsVirtual
-- indexByLine hi ln = g $ find f hi
--   where
--     f (HiVirt (MVLine _ cln)) = ln == cln
--     f _ = False
--     g (Just (HiVirt (MVLine inst _))) = inst
--     g _ = error "failed to index by line"
-- 
-- spilled :: ALR -> Bool
-- spilled (ALR tmpreg _) = case tmpreg of
--   Just _ -> False
--   Nothing -> True
-- 
-- allocatedLoadInst vreg preg = P.Lw preg (Imm "0") Fp
-- allocatedStoreInst vreg preg = P.Sw preg (Imm "0") Fp

doLoadsStores :: Function V.MipsVirtual -> [MVLine] -> [P.MipsPhys]
doLoadsStores vf = concatMap (composedALROps vf)

composedALROps alrs his 
  = (allocatedLoads alrs his) ++ (spilledLoads alrs his) 
  ++ (spilledInst alrs his) ++ (spilledStores alrs his)
  ++ (allocatedStores alrs his)

--piped inst = let x = (allocatePRegs . buildLiveRanges $ inst) in concatMap (composedALROps x) inst
piped vf = concatMap (composedALROps vf) (lined . instrs $ vf)

-- if the line is at the start of a live range that has no def
allocatedLoads :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
allocatedLoads vf (MVLine mv ln) = mapMaybe load $ filter cond alrs 
  where
    cond (ALR preg lr) = (ln == unStart lr) && (unHasDef lr == False)
    load (ALR (Just preg) lr) = Just $ P.Lw (T preg) (k $ unVReg lr) Fp
    load (ALR (Nothing) lr) = Nothing
    alrs = (allocatePRegs . buildLiveRanges . lined . instrs $ vf)
    k = toImm . (M.!) (calcRegMap vf)

-- line is at start of live range AND doStore is true
allocatedStores :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
allocatedStores vf (MVLine mv ln) = mapMaybe store $ filter cond alrs 
  where
    cond (ALR preg lr) = (ln == unStart lr) && (unNeedsStore lr == True)
    store (ALR (Just preg) lr) = Just $ P.Sw (T preg) (k $ unVReg lr) Fp
    store (ALR (Nothing) lr) = Nothing
    alrs = (allocatePRegs . buildLiveRanges . lined . instrs $ vf)
    k = toImm . (M.!) (calcRegMap vf)

-- start of a live range that has no def OR
-- live range starts strictly before line
spilledLoads :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
spilledLoads vf (MVLine mv ln) = mapMaybe load $ filter cond alrs
  where
    cond alr = (cond0 alr) || (cond1 alr)
    cond0 (ALR preg lr) = (unStart lr) == ln && (unHasDef lr) == False
    cond1 (ALR preg lr) = (unStart lr) < ln && (unEnd lr) >= ln
    load (ALR (Just preg) lr) = Nothing
    load (ALR (Nothing) lr) 
      | firstUsed mv == Just (unVReg lr) = Just $ P.Lw (M M1) (k $ unVReg lr) Fp
      | secondUsed mv == Just (unVReg lr) = Just $ P.Lw (M M2) (k $ unVReg lr) Fp
      | otherwise = Nothing
    alrs = (allocatePRegs . buildLiveRanges . lined . instrs $ vf)
    k = toImm . (M.!) (calcRegMap vf)

-- start of a live range AND hasDef
spilledStores :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
spilledStores vf (MVLine mv ln) = mapMaybe store $ filter cond alrs
  where
    cond (ALR preg lr) = (unStart lr == ln) && (unHasDef lr) == True
    store (ALR (Just preg) lr) = Nothing
    store (ALR (Nothing) lr) = Just $ P.Sw (M M1) (k $ unVReg lr) Fp -- spill stores to m1
    alrs = (allocatePRegs . buildLiveRanges . lined . instrs $ vf)
    k = toImm . (M.!) (calcRegMap vf)

spilledInst :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
spilledInst vf (MVLine mv ln) = case mv of 
  V.Addi t s i -> 
    [ P.Addi defPReg usedPreg1 i 
    ]
  V.Add d s t -> 
    [ P.Add defPReg usedPreg1 usedPreg2 
    ]
  V.Sub d s t -> 
    [ P.Sub defPReg usedPreg1 usedPreg2 
    ]
  V.SubVI t s i -> 
    [ P.Li (M M2) i
    , P.Sub defPReg usedPreg1 (M M2)
    ]
  V.SubIV t i s -> 
    [ P.Li (M M2) i
    , P.Add defPReg (M M2) usedPreg1
    ]
  V.Mult d s t -> 
    [ P.Mult usedPreg1 usedPreg2 
    , P.Mflo defPReg
    ]
  V.Multi t s i -> 
    [ P.Li (M M2) i
    , P.Mult usedPreg1 (M M2) 
    , P.Mflo defPReg
    ]
  V.Div d s t -> 
    [ P.Div usedPreg1 usedPreg2 
    , P.Mflo defPReg
    ]
  V.DivVI t s i -> 
    [ P.Li (M M2) i
    , P.Div usedPreg1 (M M2) 
    , P.Mflo defPReg
    ]
  V.DivIV t i s -> 
    [ P.Li (M M2) i
    , P.Div (M M2) usedPreg1 
    , P.Mflo defPReg
    ]
  V.Andi t s i -> 
    [ P.Andi defPReg usedPreg1 i 
    ]
  V.And d s t -> 
    [ P.And defPReg usedPreg1 usedPreg2 
    ]
  V.Ori t s i -> 
    [ P.Ori defPReg usedPreg1 i 
    ]
  V.Or d s t -> 
    [ P.Or defPReg usedPreg1 usedPreg2 
    ]

  V.Br c a b l -> case c of
    V.Eq -> 
      [ P.Beq usedPreg1 usedPreg2 l
      ]
    V.Neq -> 
      [ P.Bne usedPreg1 usedPreg2 l
      ]
    V.Lt -> 
      [ P.Sub (M M1) usedPreg1 usedPreg2
      , P.Bgtz (M M1) l
      ]
    V.Gt -> 
      [ P.Sub (M M1) usedPreg2 usedPreg1
      , P.Bgtz (M M1) l
      ]
    V.Geq -> 
      [ P.Sub (M M1) usedPreg2 usedPreg1
      , P.Blez (M M1) l
      ]
    V.Leq -> 
      [ P.Sub (M M1) usedPreg1 usedPreg2
      , P.Blez (M M1) l
      ]

  V.Bri c a i l -> case c of
    V.Eq -> 
      [ P.Li (M M2) i
      , P.Beq usedPreg1 (M M2) l
      ]
    V.Neq -> 
      [ P.Li (M M2) i
      , P.Bne usedPreg1 (M M2) l
      ]
    V.Lt -> 
      [ P.Li (M M2) i
      , P.Sub (M M1) (M M2) usedPreg1
      , P.Bgtz (M M1) l
      ]
    V.Gt -> 
      [ P.Li (M M2) i
      , P.Sub (M M1) usedPreg1 (M M2)
      , P.Bgtz (M M1) l
      ]
    V.Geq -> 
      [ P.Li (M M2) i
      , P.Sub (M M1) (M M2) usedPreg1
      , P.Blez (M M1) l
      ]
    V.Leq -> 
      [ P.Li (M M2) i
      , P.Sub (M M1) usedPreg1 (M M2)
      , P.Blez (M M1) l
      ]

  V.Lw t i s ->
    [ P.Lw defPReg i usedPreg1
    ]

  V.Li t i ->
    [ P.Li defPReg i
    ]

--  V.Sw t i s ->
--    [ P.Li defPReg i
--    ]
--
  V.Label lab -> 
    [ P.Label lab
    ]

--  V.Goto lab -> 
--    [ setupGoto lab
--    ]
  
  V.AssignI d i ->
    [ P.Li defPReg i
    ]

  V.AssignV d s ->
    [ P.Addi defPReg usedPreg1 (Imm "0")
    ]
  
  where
    k = toImm . (M.!) (calcRegMap vf)
    alrs = (allocatePRegs . buildLiveRanges . lined . instrs $ vf)
    defPReg = case filter (\(ALR _ lr) -> unStart lr == ln && unHasDef lr) alrs of
      [ALR Nothing _] -> (M M1)
      [ALR (Just preg) _] -> T preg
      otherwise -> error $ "multiple or no live ranges (with def) start at line" ++ (show ln)
    -- get preg for first arg
    usableCond lr = (unStart lr < ln || (unStart lr == ln && (unHasDef lr) == False)) && unEnd lr >= ln
    usedPreg1 = case filter (\(ALR _ lr) -> usableCond lr && Just (unVReg lr) == firstUsed mv) alrs of
      [ALR Nothing _] -> (M M1)
      [ALR (Just preg) _] -> T preg
      otherwise -> error $ "multiple or no live ranges for first used var at line" ++ (show ln)
    usedPreg2 = case filter (\(ALR _ lr) -> usableCond lr && Just (unVReg lr) == secondUsed mv) alrs of
      [ALR Nothing _] -> if firstUsed mv == secondUsed mv then (M M1) else (M M2)
      [ALR (Just preg) _] -> T preg
      otherwise -> error $ "multiple or no live ranges for first used var at line" ++ (show ln)

--insertLoadsStores hinsts alrs = foldr (composeInserts) alrs hinsts

{-

-- insert load before live range that is allocated 
-- but doesn't start with a definition
loadsAllocatedNoDef :: ALR 
                    -> [HybridInstruction] 
                    -> [HybridInstruction]
loadsAllocatedNoDef (ALR Nothing _) his = his
loadsAllocatedNoDef alr@(ALR (Just treg) lr) his
  | not (startsWithDef lr) && not (spilled alr) 
      = insertBefore (unStart lr) (loadInst (unVReg lr) (T treg)) his
  | otherwise = his
  where 
    startsWithDef lr 
      = defVar (indexByLine his $ unStart lr) == Just (unVReg lr)

-- insert a load before every read in the spilled range
loadsSpilled :: ALR 
             -> [HybridInstruction] 
             -> [HybridInstruction]
loadsSpilled (ALR Nothing lr) his 
  = undefined -- this should be done sequentially instruction-wise
  where
    releventLines = filter choose his
    choose hinst = (usesVReg $ unVReg lr $ hinst) && (inRange hinst)
    usesVReg vreg inst = vreg `elem` (usedVars inst)
    inRange (HiVirt (MVLine _ ln)) 
      = (unStart lr) <= ln && (unEnd lr) >= ln
    inRange (HIPhys _) = False
loadsSpilled (ALR (Just _) _) = id
--data LiveRange = LiveRange { unVReg :: VReg, unStart :: Line, unEnd :: Line, unUses :: Integer, unNeedsStore :: Bool } deriving Show

-- store after the first line of the range (if doStore)
storesAllocated :: ALR 
                -> [HybridInstruction] 
                -> [HybridInstruction]
storesAllocated (ALR _ lr) his
  -- we can do this even if first line isn't a def
  | (unDoStore lr) and startsWithDef
      = insertAfter (unStart lr) (loadInst (unVReg lr) (T treg)) his
  | otherwise = his
  where 
    startsWithDef
      = defVar (indexByLine his $ unStart lr) == Just (unVReg lr)

-- store after the first line of the range (if doStore OR (first instruction is def and uses > 0))
storesSpilled :: ALR -> [HybridInstruction] -> [HybridInstruction]
storesSpilled = undefined

-- convert the virtual instructions into physical
finalPass :: [HybridInstruction] -> [P.MipsPhys]
finalPass = undefined
-}

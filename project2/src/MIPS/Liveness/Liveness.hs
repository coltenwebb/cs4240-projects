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
import qualified Data.List.NonEmpty as NE

import Control.Monad.State.Lazy

import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P

import MIPS.CallingConvention
import TigerIR.Program

import qualified MIPS.Liveness.MipsCFG as MipsCFG

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
    [ V.Label (Label "hi") 
    , V.AssignV (VReg (Variable "d")) (VReg (Variable "a"))
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

-- ============
-- Driving Code
-- ============

physFnSelection :: V.VirtualFunction -> P.PhysicalFunction
physFnSelection vf = vf { instrs = selectFnInstructions vf }

selectFnInstructions :: V.VirtualFunction -> [P.MipsPhys]
selectFnInstructions vf = concatMap (selectBB vf) (MipsCFG.splitIntoBasicBlocks . instrs $ vf)

selectBB :: V.VirtualFunction -> MipsCFG.BasicBlockMips -> [P.MipsPhys]
selectBB vf bb = trace (show bb) $ selectInstructions vf . NE.toList $ MipsCFG.instrs bb

selectInstructions :: V.VirtualFunction -> [V.MipsVirtual] -> [P.MipsPhys]
selectInstructions vf vinsts = concatMap expand (lined vinsts)
  where
    alrs = allocatePRegs . buildLiveRanges . lined $ vinsts
    k = toImm . (M.!) (calcRegMap vf)
    expand mvl@(MVLine mv ln) = trace (show loads) loads ++ select ++ stores
      where
        loads = allocatedLoads alrs k mvl ++ spilledLoads alrs k mvl
        select = inst $ intermediateForm vf use def k mv
          where
            use vr = fromJust . lookup vr $ usable alrs mvl
            def vr = trace (show vr) $ fromJust . lookup vr $ defable alrs mvl
        stores = spilledStores alrs k mvl ++ allocatedStores alrs k mvl

-- ====================
-- Building Live Ranges
-- ====================

defVar :: V.MipsVirtual -> Maybe VReg
defVar mv = let (IF _ _ defd) = intermediateFormNoPhys mv in defd

usedVars :: V.MipsVirtual -> [VReg]
usedVars mv = let (IF used _ _) = intermediateFormNoPhys mv in used

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
    f vinst = use vinst . kill vinst
    kill :: MVLine -> BuildLiveRangesState -> BuildLiveRangesState
    kill (MVLine vinst line) st@(plrs, lrs) = case defVar vinst of
      Nothing -> st
      Just vreg -> case findPartialLiveRange vreg of
        Nothing -> insertUnusedLiveRange vreg
        Just plr -> insertUsedLiveRange plr vreg
      where
        findPartialLiveRange vreg = find (\plr -> Just (unPVReg plr) == defVar vinst) plrs
        insertUnusedLiveRange vreg  = (plrs, LiveRange vreg line line 0 (needsStore vreg) True : lrs)
        insertUsedLiveRange plr vreg = (filter ((/=) vreg . unPVReg) plrs, LiveRange (unPVReg plr) line (unPEnd plr) (unPUses plr) (needsStore vreg) True:lrs)
        --needsStore vreg = not $ any (\lr -> unVReg lr == vreg) lrs
        needsStore vreg = True -- always store for calling convention to work
    use :: MVLine -> BuildLiveRangesState -> BuildLiveRangesState
    -- make sure works with x = y+y
    use (MVLine vinst line) st = foldl' doUse st (nub $ usedVars vinst)
      where
        doUse :: BuildLiveRangesState -> VReg -> BuildLiveRangesState
        doUse st@(plrs, lrs) vreg
          | isVRegLive plrs vreg = usePartialLiveRange st vreg
          | otherwise            = insertPartialLiveRange st vreg
        insertPartialLiveRange (plrs, lrs) vreg = (PartialLiveRange vreg line 1 : plrs, lrs)
        usePartialLiveRange (plrs, lrs) vreg
          = ([if vreg == unPVReg plr then incPartialLiveRangeUses plr else plr | plr <- plrs], lrs)
        incPartialLiveRangeUses (PartialLiveRange vreg end uses) = PartialLiveRange vreg end (uses + 1)
    finish :: BuildLiveRangesState -> [LiveRange]
    finish (plrs, lrs) = lrs ++ [LiveRange vreg (findFirstUse vreg) end uses False False | (PartialLiveRange vreg end uses) <- plrs]
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
    f alrs lr = ALR (allocatePReg lr alrs) lr : alrs

-- if the line is at the start of a live range that has no def
--allocatedLoads :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
allocatedLoads alrs k (MVLine mv ln) = mapMaybe load $ filter acond alrs 
  where
    acond (ALR preg lr) = trace (show lr) $ (ln == unStart lr) && not (unHasDef lr)
    load (ALR (Just preg) lr) = Just $ P.Lw (T preg) (k $ unVReg lr) Fp
    load (ALR Nothing lr) = Nothing

--spilledLoads :: (Function V.MipsVirtual) -> MVLine -> [P.MipsPhys]
spilledLoads alrs k mvl@(MVLine mv ln) = map told (spilledLoadsArr alrs k mvl)
  where
    told (vreg,preg) = P.Lw preg (k vreg) Fp

-- start of a live range that has no def OR
-- live range starts strictly before line
spilledLoadsArr :: [ALR] -> (VReg -> Imm) -> MVLine -> [(VReg,PReg)]
spilledLoadsArr alrs k (MVLine mv ln) = zip (mapMaybe load $ filter scond alrs) (map M [M1 .. M4])
  where
    scond alr = (scond0 alr || scond1 alr) && scond2 alr
    scond0 (ALR preg lr) = unStart lr == ln && not (unHasDef lr)
    scond1 (ALR preg lr) = unStart lr < ln && unEnd lr >= ln
    scond2 (ALR preg lr) = unVReg lr `elem` usedVars mv
    load (ALR (Just preg) lr) = Nothing
    load (ALR Nothing lr) = Just (unVReg lr)

loadPInsts :: [ALR] -> (VReg -> Imm) -> MVLine -> [P.MipsPhys]
loadPInsts alrs k mvl = map f $ [ x|x@(_, M _) <- usable alrs mvl ]
  where
    f (vreg,preg) = P.Lw preg (k vreg) Fp

-- returns a list of (key,value) pairs mapping
-- useable (may appear on right hand side of operation)
-- vregs to pregs. spilled have M registers, allocated have
-- T registers
usable :: [ALR] -> MVLine -> [(VReg, PReg)]
usable alrs (MVLine mv ln) 
  = let toload = filter (\x -> (acond x || scond x) && cond x) alrs
    in zip [ unVReg lr | (ALR Nothing lr) <- toload ] (map M [M1 .. M4])
    ++ [ (unVReg lr, T pr) | (ALR (Just pr) lr) <- toload ]
  where
    -- does the live ranges vreg match?
    cond (ALR preg lr) = unVReg lr `elem` usedVars mv

    -- is the live range allocated during this line?
    acond (ALR preg lr) = (ln == unStart lr) && not (unHasDef lr)

    -- is the live range spilled on this line?
    scond alr = scond0 alr || scond1 alr
    scond0 (ALR preg lr) = unStart lr == ln && not (unHasDef lr)
    scond1 (ALR preg lr) = unStart lr < ln && unEnd lr >= ln

-- returns (key,value) pairs mapping
-- virtual registers that appear on the LHS 
-- to physical registers. in reality, this list will
-- either be empty or have a single element.
defable :: [ALR] -> MVLine -> [(VReg, PReg)]
defable alrs (MVLine mv ln)
  = let tostore = filter (\x -> (acond x || scond x) && cond x) alrs
    in zip [ unVReg lr | (ALR Nothing lr) <- tostore ] (map M [M1 .. M4])
    ++ [ (unVReg lr, T pr) | (ALR (Just pr) lr) <- tostore ]
  where
    cond (ALR preg lr) = Just (unVReg lr) == defVar mv
    acond (ALR preg lr) = (ln == unStart lr) && unNeedsStore lr
    scond (ALR preg lr) = (unStart lr == ln) && unHasDef lr


-- line is at start of live range AND doStore is true
--allocatedStores :: Function V.MipsVirtual -> MVLine -> [P.MipsPhys]
allocatedStores alrs k (MVLine mv ln) = mapMaybe store $ filter cond alrs 
  where
    cond (ALR preg lr) = (ln == unStart lr) && unNeedsStore lr
    store (ALR (Just preg) lr) = Just $ P.Sw (T preg) (k $ unVReg lr) Fp
    store (ALR Nothing lr) = Nothing

-- start of a live range AND hasDef
--spilledStores :: Function V.MipsVirtual -> MVLine -> [P.MipsPhys]
spilledStores alrs k (MVLine mv ln) = mapMaybe store $ filter cond alrs
  where
    cond (ALR preg lr) = (unStart lr == ln) && unHasDef lr
    store (ALR (Just preg) lr) = Nothing
    store (ALR Nothing lr) = Just $ P.Sw (M M1) (k $ unVReg lr) Fp -- spill stores to m1

-- an intermediate form approach:
-- so we create the intermediate form from mips virtual
-- it holds everything needed to do liveness, allocation and v->p

data IF = IF 
  { uses :: [VReg]
  , inst :: [P.MipsPhys]
  , defs :: Maybe VReg
  }
    
intermediateFormNoPhys :: V.MipsVirtual -> IF
intermediateFormNoPhys 
  = intermediateForm undefined undefined undefined undefined

-- max 3 items used
-- 0 or 1 items def'd
intermediateForm :: Function V.MipsVirtual
                 -> (VReg -> PReg) 
                 -> (VReg -> PReg) 
                 -> (VReg -> Imm)
                 -> V.MipsVirtual
                 -> IF
intermediateForm vf use def k mv = case mv of 
  V.Addi t s i -> 
    IF { uses = [s]
       , inst =
         [ P.Addi (def t) (use s) i 
         ]
       , defs = Just t
       }
  V.Add d s t -> 
    IF { uses = [s, t]
       , inst =
         [ P.Add (def d) (use s) (use t) 
         ]
       , defs = Just d
       }
  V.Sub d s t -> 
    IF { uses = [s, t]
       , inst =
         [ P.Sub (def d) (use s) (use t) 
         ]
       , defs = Just d
       }
  V.SubVI t s i -> 
    IF { uses = [s]
       , inst =
         [ P.Li (M M2) i
         , P.Sub (def t) (use s) (M M2) 
         ]
       , defs = Just t
       }
  V.SubIV t i s -> 
    IF { uses = [s]
       , inst =
         [ P.Li (M M2) i
         , P.Sub (def t) (M M2) (use s)
         ]
       , defs = Just t
       }
  V.Mult d s t -> 
    IF { uses = [s, t]
       , inst =
         [ P.Mult (use s) (use t) 
         , P.Mflo (def d)
         ]
       , defs = Just d
       }
  V.Multi t s i -> 
    IF { uses = [s]
       , inst =
         [ P.Li (M M2) i
         , P.Mult (use s) (M M2) 
         , P.Mflo (def t)
         ]
       , defs = Just t
       }
  V.Div d s t -> 
    IF { uses = [s, t]
       , inst =
         [ P.Div (use s) (use t) 
         , P.Mflo (def d)
         ]
       , defs = Just d
       }
  V.DivVI t s i -> 
    IF { uses = [s]
       , inst =
         [ P.Li (M M2) i
         , P.Mult (use s) (M M2) 
         , P.Mflo (def t)
         ]
       , defs = Just t
       }
  V.DivIV t i s -> 
    IF { uses = [s]
       , inst =
         [ P.Li (M M2) i
         , P.Mult (M M2) (use s)
         , P.Mflo (def t)
         ]
       , defs = Just t
       }
  V.Andi t s i -> 
    IF { uses = [s]
       , inst =
         [ P.Andi (def t) (use s) i 
         ]
       , defs = Just t
       }
  V.And d s t -> 
    IF { uses = [s, t]
       , inst =
         [ P.And (def d) (use s) (use t) 
         ]
       , defs = Just d
       }
  V.Ori t s i -> 
    IF { uses = [s]
       , inst =
         [ P.Ori (def t) (use s) i 
         ]
       , defs = Just t
       }
  V.Or d s t -> 
    IF { uses = [s, t]
       , inst =
         [ P.Or (def d) (use s) (use t) 
         ]
       , defs = Just d
       }
  V.Br c a b l -> case c of
    V.Eq ->
      IF { uses = [a, b]
         , inst =
           [ P.Beq (use a) (use b) l
           ]
         , defs = Nothing
         }
    V.Neq ->
      IF { uses = [a, b]
         , inst =
           [ P.Bne (use a) (use b) l
           ]
         , defs = Nothing
         }
    V.Lt ->
      IF { uses = [a, b]
         , inst =
           [ P.Sub (M M4) (use b) (use a)
           , P.Bgtz (M M4) l
           ]
         , defs = Nothing
         }
    V.Gt ->
      IF { uses = [a, b]
         , inst =
           [ P.Sub (M M4) (use a) (use b)
           , P.Bgtz (M M4) l
           ]
         , defs = Nothing
         }
    V.Geq ->
      IF { uses = [a, b]
         , inst =
           [ P.Sub (M M4) (use b) (use a)
           , P.Blez (M M4) l
           ]
         , defs = Nothing
         }
    V.Leq ->
      IF { uses = [a, b]
         , inst =
           [ P.Sub (M M4) (use a) (use b)
           , P.Blez (M M4) l
           ]
         , defs = Nothing
         }
  V.Bri c a i l -> case c of
    V.Eq ->
      IF { uses = [a]
         , inst =
           [ P.Li (M M4) i
           , P.Beq (use a) (M M4) l
           ]
         , defs = Nothing
         }
    V.Neq ->
      IF { uses = [a]
         , inst =
           [ P.Li (M M4) i
           , P.Bne (use a) (M M4) l
           ]
         , defs = Nothing
         }
    V.Lt ->
      IF { uses = [a]
         , inst =
           [ P.Li (M M4) i
           , P.Sub (M M4) (M M4) (use a)
           , P.Bgtz (M M4) l
           ]
         , defs = Nothing
         }
    V.Gt ->
      IF { uses = [a]
         , inst =
           [ P.Li (M M4) i
           , P.Sub (M M4) (use a) (M M4)
           , P.Bgtz (M M4) l
           ]
         , defs = Nothing
         }
    V.Geq ->
      IF { uses = [a]
         , inst =
           [ P.Li (M M4) i
           , P.Sub (M M4) (M M4) (use a)
           , P.Blez (M M4) l
           ]
         , defs = Nothing
         }
    V.Leq ->
      IF { uses = [a]
         , inst =
           [ P.Li (M M4) i
           , P.Sub (M M4) (use a) (M M4)
           , P.Blez (M M4) l
           ]
         , defs = Nothing
         }

  V.Lw t i s -> undefined
  V.Sw t i s -> undefined
  V.Li d i -> 
    IF { uses = []
       , inst =
         [ P.Li (use d) i
         ]
       , defs = Just d
       }
  V.Label lab -> 
    IF { uses = []
       , inst =
         [ P.Label lab
         ]
       , defs = Nothing
       }
  V.Goto lab ->
    IF { uses = []
       , inst = setupGoto lab
       , defs = Nothing
       }

  V.Call fn args -> 
    IF { uses = [ vreg | (V.CVarg vreg) <- args ]
       , inst = setupCallStack fn args loadReg
       , defs = Nothing
       }

  V.Callr retvreg fn args -> 
    IF { uses = [ vreg | (V.CVarg vreg) <- args ]
       , inst = setupCallStack fn args loadReg
             ++ [ P.Addi (def retvreg) Retval (Imm "0") ]
       , defs = Just retvreg
       }
  V.AssignI d i -> 
    IF { uses = []
       , inst =
         [ P.Li (def d) i
         ]
       , defs = Just d
       }
  V.AssignV d s -> 
    IF { uses = [s]
       , inst =
         [ P.Addi (def d) (use s) (Imm "0")
         ]
       , defs = Just d
       }

  -- solution to array problems: use a couple s registers :/
  -- a[i] = s
  V.ArrStr s a i ->
    IF { uses = [s, a, i]
       , inst =
         [ P.Addi (M M4) ZeroReg (Imm "-4") -- this should left shift
         , P.Mult (M M4) (use i)
         , P.Mflo (M M4) -- m1=4*i
         , P.Add (M M4) (M M4) (use a)
         , P.Sw (use s) (Imm "0") (M M4) -- mem[m1]=m2
         ]
       , defs = Nothing
       }
  V.ArrStriv s a i ->
    IF { uses = [a, i]
       , inst =
         [ P.Addi (M M4) ZeroReg (Imm "-4")
         , P.Mult (M M4) (use i)
         , P.Mflo (M M4)
         , P.Add (M M4) (M M4) (use a)
         , P.Li (M M3) s
         , P.Sw (M M3) (Imm "0") (M M4)
         ]
       , defs = Nothing
       }
  V.ArrStri s a i ->
    IF { uses = [s, a]
       , inst =
         [ P.Sw (use s) (times4 i) (use a)
         ]
       , defs = Nothing
       }
  V.ArrStrii s a i ->
    IF { uses = [a]
       , inst =
         [ P.Li (M M4) s
         , P.Sw (M M4) (times4 i) (use a)
         ]
       , defs = Nothing
       }
  -- s = a[i]
  V.ArrLoad s a i ->
    IF { uses = [a, i]
       , inst =
         [ P.Addi (M M4) ZeroReg (Imm "-4")
         , P.Mult (M M4) (use i)
         , P.Mflo (M M4)
         , P.Add (M M4) (M M4) (use a)
         , P.Lw (def s) (Imm "0") (M M4)
         ]
       , defs = Just s
       }
  V.ArrLoadi s a i ->
    IF { uses = [a]
       , inst =
         [ P.Lw (def s) (times4 i) (use a) -- mem[m1]=m2
         ]
       , defs = Just s
       }
  V.Nop ->
    IF { uses = []
       , inst = []
       , defs = Nothing
       }
  V.Return r ->
    IF { uses = [r]
       , inst = setupReturn r loadReg
       , defs = Nothing
       }
  V.Returni i ->
    IF { uses = []
       , inst = setupReturnImm i
       , defs = Nothing
       }
  V.BeginFunction ->
    IF { uses = []
       , inst = fnEntry vf rm
       , defs = Nothing
       }
  V.EndFunction ->
    IF { uses = []
       , inst = setupReturnVoid
       , defs = Nothing
       }
  
  V.ArrAssignVV v1 v2 v3 -> 
    IF { uses = [v1, v2, v3]
       , inst = setupCallStack (Label "memset") (map V.CVarg [v1, v2, v3]) loadReg
       , defs = Nothing
       }

  V.ArrAssignVI v1 v2 i3 -> 
    IF { uses = [v1, v2]
       , inst = setupCallStack (Label "memset") [V.CVarg v1, V.CVarg v2, V.CIarg i3] loadReg
       , defs = Nothing
       }

  V.ArrAssignII v1 i2 i3 -> 
    IF { uses = [v1]
       , inst = setupCallStack (Label "memset") [V.CVarg v1, V.CIarg i2, V.CIarg i3] loadReg       
       , defs = Nothing
       }

  V.ArrAssignIV v1 i2 v3 -> 
    IF { uses = [v1, v3]
       , inst = setupCallStack (Label "memset") [V.CVarg v1, V.CIarg i2, V.CVarg v3] loadReg       
       , defs = Nothing
       }

  where
    -- using this may be inefficient
    -- will be safe since we always
    -- store new values immediately
    loadReg :: VReg -> (PReg, [P.MipsPhys])
    loadReg vreg = (M M4, [ P.Lw (M M4) (k vreg) Fp ])
    times4 :: Imm -> Imm
    times4 (Imm i) = Imm (show $ (read i :: Int) * (-4))
    rm :: RegMap
    rm = calcRegMap vf

--  where
--    k = toImm . (M.!) (calcRegMap vf)
--    alrs = (allocatePRegs . buildLiveRanges . lined . instrs $ vf)
--    -- defPReg = case filter (\(ALR _ lr) -> unStart lr == ln && unHasDef lr) alrs of
--    --   [ALR Nothing _] -> (M M1)
--    --   [ALR (Just preg) _] -> T preg
--    --   otherwise -> error $ "multiple or no live ranges (with def) start at line" ++ (show ln)
--    -- get preg for first arg
--    usableCond lr = (unStart lr < ln || (unStart lr == ln && (unHasDef lr) == False)) && unEnd lr >= ln
--    -- usedPreg1 = case filter (\(ALR _ lr) -> usableCond lr && Just (unVReg lr) == firstUsed mv) alrs of
--    --   [ALR Nothing _] -> (M M1)
--    --   [ALR (Just preg) _] -> T preg
--    --   otherwise -> error $ "multiple or no live ranges for first used var at line" ++ (show ln)
--    -- usedPreg2 = case filter (\(ALR _ lr) -> usableCond lr && Just (unVReg lr) == secondUsed mv) alrs of
--    --   [ALR Nothing _] -> if firstUsed mv == secondUsed mv then (M M1) else (M M2)
--    --   [ALR (Just preg) _] -> T preg
--    --   otherwise -> error $ "multiple or no live ranges for first used var at line" ++ (show ln)
--
--    def vreg = case filter (\(ALR _ lr) -> unStart lr == ln && unHasDef lr && unVReg lr == vreg) alrs of
--      [ALR Nothing _] -> (M M1)
--      [ALR (Just preg) _] -> T preg
--      otherwise -> error $ "multiple or no live ranges (with def) start at line" ++ (show ln)
--    use vreg = case filter (\(ALR _ lr) -> usableCond lr && unVReg lr == vreg) alrs of
--      [ALR Nothing _] -> (M M1)
--      [ALR (Just preg) _] -> T preg
--      otherwise -> error $ "multiple or no live ranges for first used var at line" ++ (show ln)
--
--    loadReg :: VReg -> (PReg, [P.MipsPhys])
--    loadReg vreg =
--      (M M1, [ P.Lw (M M1) (k vreg) Fp ])
--    times4 :: Imm -> Imm
--    times4 (Imm i) = Imm (show $ (read i :: Int) * 4)
--

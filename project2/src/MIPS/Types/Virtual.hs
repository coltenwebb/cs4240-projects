module MIPS.Types.Virtual where

import MIPS.Types.Operand
import Data.Maybe

import TigerIR.Program
import TigerIR.IrInstruction (BrOp(..))

type VirtualFunction = Function MipsVirtual
type VirtualProgram  = Program  MipsVirtual

-- [P] denotes pseudo instr
-- pushin [P]
data MipsVirtual
  = Assign   VReg VReg
  | Li       VReg Imm          -- [P]
  -- Commutative bin-ops
  | Addi     VReg VReg Imm  
  | Add      VReg VReg VReg    -- 
  | Mult     VReg VReg VReg
  | Multi    VReg VReg Imm     -- [P]
  | Andi     VReg VReg Imm
  | And      VReg VReg VReg
  | Ori      VReg VReg Imm
  | Or       VReg VReg VReg
  -- Special case for non-commutativity
  | Sub      VReg VReg VReg
  | SubVI    VReg VReg Imm     -- [P]
  | SubIV    VReg Imm  VReg
  | Div      VReg VReg VReg
  | DivVI    VReg VReg Imm
  | DivIV    VReg Imm  VReg

  | BrVI     BrOp VReg Imm  Label
  | BrVV     BrOp VReg VReg Label
  | BrIV     BrOp Imm  VReg Label
  | BrII     BrOp Imm  Imm  Label

  | LabelIns Label
  | Goto     Label
  | Call     Label [CallArg]      -- [P]
  | Callr    VReg Label [CallArg] -- [P]
  -- Same convention as TigerIR
  -- array_store, a, arr, 0
  -- arr[0] := a
  | ArrStrVAV  VReg VReg VReg
  | ArrStrVAI  VReg VReg Imm
  | ArrStrIAI  Imm  VReg Imm 
  | ArrStrIAV  Imm  VReg VReg
  -- array_load, a, arr, 0
  -- a := arr[0]
  | ArrLoadV VReg VReg VReg
  | ArrLoadI VReg VReg Imm
  -- assign, X, 100, 10
  -- type ArrayInt = array [100] of int;
  -- var X : ArrayInt := 10
  | ArrAssignAII  VReg Imm  Imm
  | ArrAssignAIV  VReg Imm  VReg
  | ArrAssignAVI  VReg VReg Imm
  | ArrAssignAVV  VReg VReg VReg
  | Nop                           -- [P]
  | Return   VReg                 -- [P]
  | Returni  Imm                  -- [P]
  | BeginFunction                 -- [P] for initialization
  | EndFunction                   -- [P] void return
  deriving Show

data CallArg = CVarg VReg | CIarg Imm deriving Show

getDef :: MipsVirtual -> Maybe VReg
getDef mv = case mv of
  Assign d _     -> Just d
  Li     d _     -> Just d
  Addi   d _ _   -> Just d
  Add    d _ _   -> Just d
  Mult   d _ _   -> Just d
  Multi  d _ _   -> Just d
  Andi   d _ _   -> Just d 
  And    d _ _   -> Just d   
  Ori    d _ _   -> Just d  
  Or     d _ _   -> Just d 
  Sub    d _ _   -> Just d 
  SubVI  d _ _   -> Just d 
  SubIV  d _ _   -> Just d 
  Div    d _ _   -> Just d 
  DivVI  d _ _   -> Just d 
  DivIV  d _ _   -> Just d 

  BrVI      {}   -> Nothing
  BrVV      {}   -> Nothing
  BrIV      {}   -> Nothing
  BrII      {}   -> Nothing

  LabelIns   _   -> Nothing
  Goto       _   -> Nothing
  Call      {}   -> Nothing
  Callr  d _ _   -> Just d

  ArrStrVAV  {}  -> Nothing  
  ArrStrVAI  {}  -> Nothing 
  ArrStrIAI  {}  -> Nothing 
  ArrStrIAV  {}  -> Nothing 

  ArrLoadV d _ _ -> Just d
  ArrLoadI d _ _ -> Just d

  ArrAssignAII {} -> Nothing
  ArrAssignAIV {} -> Nothing
  ArrAssignAVI {} -> Nothing
  ArrAssignAVV {} -> Nothing
  Nop             -> Nothing
  Return      _   -> Nothing
  Returni     _   -> Nothing
  BeginFunction   -> Nothing
  EndFunction     -> Nothing
  
getUses :: MipsVirtual -> [VReg]
getUses mv = case mv of
  Assign _ a       -> [a]
  Li     {}        -> []
  Addi   _ a _     -> [a]
  Add    _ a b     -> [a,b]
  Mult   _ a b     -> [a,b]
  Multi  _ a _     -> [a]
  Andi   _ a _     -> [a]
  And    _ a b     -> [a,b]
  Ori    _ a _     -> [a]
  Or     _ a b     -> [a,b]
  Sub    _ a b     -> [a,b]
  SubVI  _ a _     -> [a]
  SubIV  _ _ a     -> [a]
  Div    _ a b     -> [a,b]
  DivVI  _ a _     -> [a]
  DivIV  _ _ a     -> [a]

  BrVI _ a _ _     -> [a]
  BrVV _ a b _     -> [a,b]
  BrIV _ _ a _     -> [a]
  BrII {}          -> []

  LabelIns   _     -> []
  Goto       _     -> []
  Call    _ cas    -> getCallArgUses cas
  Callr a _ cas    -> a : getCallArgUses cas

  ArrStrVAV a b c  -> [a,b,c]
  ArrStrVAI a b _  -> [a,b]
  ArrStrIAI _ a _  -> [a]
  ArrStrIAV _ a b  -> [a,b]

  ArrLoadV a b c   -> [a,b,c]
  ArrLoadI a b _   -> [a,b]

  ArrAssignAII a _ _ -> [a]
  ArrAssignAIV a _ b -> [a,b]
  ArrAssignAVI a b _ -> [a,b]
  ArrAssignAVV a b c -> [a,b,c]


  Nop              -> []
  Return     a     -> [a]
  Returni    _     -> []
  BeginFunction    -> []
  EndFunction      -> []
  where
    getCallArgUses :: [CallArg] -> [VReg]
    getCallArgUses cas = flip concatMap cas $ \ca ->
      case ca of
        CVarg vr -> [vr]
        CIarg _  -> []
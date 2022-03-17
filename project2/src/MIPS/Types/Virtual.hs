module MIPS.Types.Virtual where

import MIPS.Types.Operand
import TigerIR.Types (Imm, Label)
import Data.Maybe

-- [P] denotes pseudo instr (incomplete atm, some pseudo not annotated)
-- pushin [P]

class Mips a where
  isBranching :: a -> Bool
  isLabel     :: a -> Bool
  isReturnOp  :: a -> Bool
  isGotoOp    :: a -> Bool
  -- If it is a branch / goto / etc. it will have a label
  -- to jump to, used for determining consecutive blocks 

data MipsVirtual
  = AssignI  VReg Imm
  | AssignV  VReg VReg
  | Addi     VReg VReg Imm
  | Li       VReg Imm          -- [P]
  | Add      VReg VReg VReg    -- 
  | Sub      VReg VReg VReg
  | Subi     VReg VReg Imm     -- [P]
  | Mult     VReg VReg VReg
  | Multi    VReg VReg Imm     -- [P]
  | Div      VReg VReg VReg
  | Divi     VReg VReg Imm
  | Andi     VReg VReg Imm
  | And      VReg VReg VReg
  | Ori      VReg VReg Imm
  | Or       VReg VReg VReg
  | Bri      Cmp  VReg Imm  Label
  | Br       Cmp  VReg VReg Label
  | Lw       VReg Imm VReg
  | Sw       VReg Imm VReg
  | Label    Label
  | Goto     Label
  | Call     Label [CallArg]      -- [P]
  | Callr    VReg Label [CallArg] -- [P]
  -- Same convention as TigerIR
  -- array_store, a, arr, 0
  -- arr[0] := a
  | ArrStr   VReg VReg VReg
  | ArrStri  VReg VReg Imm
  | ArrStrii Imm VReg Imm
  | ArrStriv Imm VReg VReg
  -- array_load, a, arr, 0
  -- a := arr[0]
  | ArrLoad  VReg VReg VReg
  | ArrLoadi VReg VReg Imm
  -- assign, X, 100, 10
  -- type ArrayInt = array [100] of int;
  -- var X : ArrayInt := 10
  | ArrAssignVV   VReg VReg VReg
  | ArrAssignVI   VReg VReg Imm
  | ArrAssignII   VReg Imm  Imm
  | ArrAssignIV   VReg Imm  VReg
  | Nop                           -- [P]
  | Return   VReg                 -- [P]
  | Returni  Imm                  -- [P]
  | EndFunction                   -- [P] void return
  deriving (Show)


instance Mips MipsVirtual where
  isBranching Br {}       = True
  isBranching Bri {}      = True
  isBranching _           = False 

  isLabel MIPS.Types.Virtual.Label {} = True 
  isLabel _               = False 

  isReturnOp Return {}    = True 
  isReturnOp Returni {}   = True 
  isReturnOp _            = False 

  isGotoOp Goto {}        = True 
  isGotoOp _              = False


data Cmp = Eq | Neq | Lt | Gt | Geq | Leq deriving (Show)

data CallArg = CVarg VReg | CIarg Imm deriving (Show)

newtype VirtualProgram = VirtualProgram { virtualInstructions :: [VirtualFunction] } deriving ()
data VirtualFunction = VirtualFunction { unMipsVirtuals :: [MipsVirtual], unFname :: FunctionName } deriving (Show)

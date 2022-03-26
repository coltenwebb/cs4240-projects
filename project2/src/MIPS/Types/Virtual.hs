module MIPS.Types.Virtual where

import MIPS.Types.Operand
import Data.Maybe

import TigerIR.Program
import TigerIR.IrInstruction

type VirtualFunction = Function MipsVirtual
type VirtualProgram  = Program  MipsVirtual

-- [P] denotes pseudo instr (incomplete atm, some pseudo not annotated)
-- pushin [P]
data MipsVirtual
  = AssignI  VReg Imm         
  | AssignV  VReg VReg
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
  | ArrStrVV  VReg VReg VReg
  | ArrStrVI  VReg VReg Imm
  | ArrStrII  Imm VReg Imm 
  | ArrStrIV  Imm VReg VReg
  -- array_load, a, arr, 0
  -- a := arr[0]
  | ArrLoadV VReg VReg VReg
  | ArrLoadI VReg VReg Imm
  -- assign, X, 100, 10
  -- type ArrayInt = array [100] of int;
  -- var X : ArrayInt := 10
  | ArrAssignI   VReg Imm  Imm
  | ArrAssignV   VReg Imm  VReg
  | Nop                           -- [P]
  | Return   VReg                 -- [P]
  | Returni  Imm                  -- [P]
  | BeginFunction                 -- [P] for initialization
  | EndFunction                   -- [P] void return

data Cmp = Eq | Neq | Lt | Gt | Geq | Leq deriving (Show)

data CallArg = CVarg VReg | CIarg Imm

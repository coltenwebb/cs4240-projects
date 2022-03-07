module MIPS.Types.Virtual where

import MIPS.Types.Operands
import Data.Maybe
data MipsVirtual
  = Addi     VReg VReg Imm
  | Add      VReg VReg VReg
  | Sub      VReg VReg VReg
  | Mult     VReg VReg VReg
  | Div      VReg VReg VReg
  | Andi     VReg VReg Imm
  | And      VReg VReg VReg
  | Ori      VReg VReg Imm
  | Or       VReg VReg VReg
  | Beq      VReg VReg Lab
  | Bne      VReg VReg Lab
  | Bgtz     VReg Lab
  | Lw       VReg Imm VReg
  | Sw       VReg Imm VReg
  | Label    Lab
  | Goto     Lab
  | Call     Lab  [VReg]
  | Callr    VReg Lab [VReg] 
  | Return   (Maybe VReg)

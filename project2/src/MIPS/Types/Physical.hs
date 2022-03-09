module MIPS.Types.Physical where

import MIPS.Types.Operand

data MipsPhys
  = Addi  PReg PReg Imm
  | Add   PReg PReg PReg
  | Sub   PReg PReg PReg
  | Mflo  PReg
  | Mult  PReg PReg
  | Div   PReg PReg
  | Andi  PReg PReg Imm
  | And   PReg PReg PReg
  | Ori   PReg PReg Imm
  | Or    PReg PReg PReg
  | Beq   PReg PReg Lab
  | Bne   PReg PReg Lab
  | Bgtz  PReg Lab
  | Blez  PReg Lab
  | Lw    PReg Imm PReg
  | Sw    PReg Imm PReg
  | Label Lab
  | Jal   Lab
  | Jr    PReg
  | J     Lab
  | Syscall
  | Li    PReg Imm

newtype PhysicalProgram = PhysicalProgram { physicalInstructions :: [MipsPhys] } deriving ()
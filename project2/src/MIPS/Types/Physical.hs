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
  | Beq   PReg PReg Label
  | Bne   PReg PReg Label
  | Bgtz  PReg Label
  | Blez  PReg Label
  | Lw    PReg Imm PReg
  | Sw    PReg Imm PReg
  | Label Label
  | Jal   Label
  | Jr    PReg
  | J     Label
  | Syscall
  | Li    PReg Imm

newtype PhysicalProgram = PhysicalProgram { physicalInstructions :: [MipsPhys] } deriving ()
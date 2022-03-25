module MIPS.Types.Physical where

import MIPS.Types.Operand
import TigerIR.Program
import TigerIR.IrInstruction

type PhysicalProgram  = Program  MipsPhys
type PhysicalFunction = Function MipsPhys

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
  | Lw    PReg Imm  PReg
  | Sw    PReg Imm  PReg
  | Sll   PReg PReg Imm
  | Label Label
  | Jal   Label
  | Jr    PReg
  | J     Label
  | Syscall
  | Li    PReg Imm
  deriving Show
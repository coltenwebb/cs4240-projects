module MIPS.Types.Operands where

-- Virtual Registers
newtype VReg    = VReg    Int    deriving (Show, Eq, Ord)

-- Immediate Values and Labels
newtype Imm     = Imm     String deriving (Show, Eq)
newtype Lab     = Lab     String deriving (Show, Eq)


-- Physical Register Types
-- Physical registers
data PReg
  = ZeroReg
  | ImmReg
  | RetvalReg
  | Sp
  | Fp
  | T TmpReg
  | M MReg

data TmpReg
  = T0 | T1 | T2 | T3
  | T4 | T5 | T6 | T7

-- The M registers will be reserved for holding the values of spilled
-- registers when they are used in an instruction.
-- For ex w/ v1 <- addi v2, v3, we would do
-- lw m1, $addr_of_m2
-- lw m2, $addr_of_m3
-- m1 <- addi m1, m2
-- sw k1, $addr_of_v1
data MReg = M1 | M2

data RetAddrReg = RetAddrReg

module MIPS.Types.Operand where

-- Virtual Registers
newtype VReg    = VReg    Int    deriving (Show, Eq, Ord)

-- Immediate Values and Labels
newtype Imm     = Imm     String deriving (Show, Eq)
newtype Lab     = Lab     String deriving (Show, Eq)


-- Physical registers
-- pg. 10: https://pages.cs.wisc.edu/~larus/SPIM/spim_documentation.pdf
data PReg
  = ZeroReg
  | RetAddr
  | Retval
  | SyscallCode
  | Sp
  | Fp
  | T TmpReg
  | M MReg
  | A ArgReg
  | V0  -- where result of read_int syscall is stored

data TmpReg
  = T0 | T1 | T2 | T3
  | T4 | T5 | T6 | T7

data ArgReg
  = A0 | A1 | A2 | A3

-- The M registers will be reserved for holding the values of spilled
-- registers when they are used in an instruction.
-- Corresponds to $t8, $t9
-- For ex w/ v1 <- addi v2, v3, we would do
-- lw m1, $addr_of_m2
-- lw m2, $addr_of_m3
-- m1 <- addi m1, m2
-- sw k1, $addr_of_v1
data MReg = M1 | M2

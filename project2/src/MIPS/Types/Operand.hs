module MIPS.Types.Operand
  ( module MIPS.Types.Operand
  , module TigerIR.Types
  ) where

import TigerIR.Types

-- Virtual Registers
newtype VReg = VReg Variable deriving (Show, Eq, Ord)

class PseudoReg a where
  toVReg :: a -> VReg

instance PseudoReg ParamVar where
  toVReg param = case param of
    ParamV v -> VReg v
    ParamA (Array v _) -> VReg v

instance PseudoReg LocalVar where
  toVReg param = case param of
    LocalV v -> VReg v
    LocalA (Array v _) -> VReg v

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
  deriving Show

data TmpReg
  = T0 | T1 | T2 | T3
  | T4 | T5 | T6 | T7
  deriving Show

data ArgReg
  = A0 | A1 | A2 | A3
  deriving Show

-- The M registers will be reserved for holding the values of spilled
-- registers when they are used in an instruction.
-- Corresponds to $t8, $t9
-- For ex w/ v1 <- addi v2, v3, we would do
-- lw m1, $addr_of_m2
-- lw m2, $addr_of_m3
-- m1 <- addi m1, m2
-- sw k1, $addr_of_v1
data MReg = M1 | M2
  deriving Show
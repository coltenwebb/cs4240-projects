module MIPS.Intrinsics where

import qualified MIPS.Types.Physical as P
import MIPS.Types.Operand

-- These built-in functions all assume our self-defined
-- calling convention.

-- https://pages.cs.wisc.edu/~larus/SPIM/spim_documentation.pdf
-- syscall nums pg. 8
data SpimSyscall = PrintInt | PrintString | ReadInt | ReadString

syscallNum :: SpimSyscall -> Imm
syscallNum s = case s of
  PrintInt -> Imm "1"
  PrintString -> Imm "4"
  ReadInt -> Imm "5"
  ReadString -> Imm "8"

-- Print the integer i to standard output.
-- void puti(int i)
puti :: [P.MipsPhys]
puti =
  [ P.Lw (A A0) (Imm "-4") Sp
  , P.Li SyscallCode (syscallNum PrintInt)
  , P.Syscall
  , P.Jr RetAddr
  ]

{-
  Print the character encoded as the ASCII value c to standard output.
  void putc(int c)

  We have to add the null byte ourselves, so that the syscall fn
  knows where the string ends.

      ----------------------
      sp ->   null byte         End of String
      ----------------------
             arg c          
      ----------------------
-}
putc :: [P.MipsPhys]
putc =
  [ P.Sw ZeroReg (Imm "0") Sp   -- Fill in the null byte
  , P.Addi (A A0) Sp (Imm "-4") -- Point to str
  , P.Li SyscallCode (syscallNum PrintString)
  , P.Syscall
  , P.Jr RetAddr
  ]


{-
  Read a character as its ASCII value from standard input.
  int getc()

  We have to provide the buffer ourselves, directly alloc on stack
      ----------------------
        null byte
      ----------------------
sp ->   char we read         End of String
      ----------------------

  $a0 = buffer, $a1 = length

-}
getc :: [P.MipsPhys]
getc =
  [ P.Add (A A0) Sp ZeroReg   -- buffer addr
  , P.Li  (A A1) (Imm "2")    -- length
  , P.Li SyscallCode (syscallNum ReadString)
  , P.Syscall
  , P.Lw Sp (Imm "0") Retval
  , P.Jr RetAddr
  ]

-- Read an integer from standard input.
geti :: [P.MipsPhys]
geti =
  [ P.Li SyscallCode  (syscallNum ReadInt)
  , P.Syscall
  , P.Add Retval V0 ZeroReg -- Result stored in $v0
  , P.Jr RetAddr
  ]
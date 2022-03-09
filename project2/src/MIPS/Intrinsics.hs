module MIPS.Intrinsics where

import qualified MIPS.Types.Physical as P
import MIPS.Types.Operand

-- These built-in functions all assume our self-defined
-- calling convention.

-- TODO:
-- Read https://courses.missouristate.edu/kenvollmar/mars/help/syscallhelp.html
-- Has info about print character syscall? and reach character?

-- https://pages.cs.wisc.edu/~larus/SPIM/spim_documentation.pdf
-- syscall nums pg. 8
data SpimSyscall = PrintInt | PrintChar | ReadInt | ReadChar

syscallNum :: SpimSyscall -> Imm
syscallNum s = case s of
  PrintInt -> Imm "1"
  PrintChar -> Imm "11"
  ReadInt -> Imm "5"
  ReadChar -> Imm "12"

-- Print the integer i to standard output.
-- void puti(int i)
puti :: [P.MipsPhys]
puti =
  [ P.Lw (A A0) (Imm "0") Sp  -- arg i
  , P.Li SyscallCode (syscallNum PrintInt)
  , P.Syscall
  , P.Jr RetAddr
  ]

{-
  Print the character encoded as the ASCII value c to standard output.
  void putc(int c)
  argument:  $a0 = character to print
-}
putc :: [P.MipsPhys]
putc =
  [ P.Lw (A A0) (Imm "0") Sp    -- arg c
  , P.Li SyscallCode (syscallNum PrintChar)
  , P.Syscall
  , P.Jr RetAddr
  ]
{-
  Read a character as its ASCII value from standard input.
  int getc()

  result: $v0 contains character read
-}
getc :: [P.MipsPhys]
getc =
  [ P.Li SyscallCode (syscallNum ReadChar)
  , P.Syscall
  , P.Add Retval V0 ZeroReg     -- store char into retval
  , P.Jr RetAddr
  ]

-- Read an integer from standard input.
geti :: [P.MipsPhys]
geti =
  [ P.Li SyscallCode (syscallNum ReadInt)
  , P.Syscall
  , P.Add Retval V0 ZeroReg -- Result stored in $v0
  , P.Jr RetAddr
  ]
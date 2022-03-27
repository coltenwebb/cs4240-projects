module MIPS.Intrinsics where

import qualified MIPS.Types.Physical as P
import MIPS.Types.Operand

import TigerIR.Program

intrinsicFunctions :: [P.PhysicalFunction]
intrinsicFunctions = [ puti, putc, getc, geti ]

-- These built-in functions all assume our self-defined
-- calling convention.

-- TODO:
-- Read https://courses.missouristate.edu/kenvollmar/mars/help/syscallhelp.html
-- Has info about print character syscall? and reach character?

-- https://pages.cs.wisc.edu/~larus/SPIM/spim_documentation.pdf
-- syscall nums pg. 8
data SpimSyscall = PrintInt | PrintChar | ReadInt | ReadChar | Sbrk

syscallNum :: SpimSyscall -> Imm
syscallNum s = case s of
  PrintInt -> Imm "1"
  PrintChar -> Imm "11"
  ReadInt -> Imm "5"
  Sbrk -> Imm "9"
  ReadChar -> Imm "12"

loadSyscall :: SpimSyscall -> P.MipsPhys
loadSyscall sc = P.Li SyscallCode (syscallNum sc)

-- Print the integer i to standard output.
-- void puti(int i)
puti :: P.PhysicalFunction
puti = Function
  (FunctionName (Label "puti"))
  False -- No return value
  [ParamV (Variable "i")]
  [] -- No local vars
  [ P.Lw (A A0) (Imm "0") Sp  -- arg i
  , loadSyscall PrintInt
  , P.Syscall
  , P.Jr RetAddr
  ]

{-
  Print the character encoded as the ASCII value c to standard output.
  void putc(int c)
  argument:  $a0 = character to print
-}
putc :: P.PhysicalFunction
putc = Function
  (FunctionName (Label "putc"))
  False -- No return value
  [ParamV (Variable "i")]
  [] -- No local vars
  [ P.Lw (A A0) (Imm "0") Sp    -- arg c
  , loadSyscall PrintChar
  , P.Syscall
  , P.Jr RetAddr
  ]

{-
  Read a character as its ASCII value from standard input.
  int getc()

  result: $v0 contains character read
-}
getc :: P.PhysicalFunction
getc = Function
  (FunctionName (Label "getc"))
  True -- Return ASCII value of char
  []   -- No params
  []   -- No local var
  [ loadSyscall ReadChar
  , P.Syscall
  , P.Add Retval V0 ZeroReg     -- store char into retval
  , P.Jr RetAddr
  ]

-- Read an integer from standard input.
geti :: P.PhysicalFunction
geti = Function
  (FunctionName (Label "geti"))
  True -- Return value Int
  [] -- No params
  [] -- No local var
  [ loadSyscall ReadInt
  , P.Syscall
  , P.Add Retval V0 ZeroReg -- Result stored in $v0
  , P.Jr RetAddr
  ]

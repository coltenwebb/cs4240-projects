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
data SpimSyscall = PrintInt | PrintChar | ReadInt | ReadChar

syscallNum :: SpimSyscall -> Imm
syscallNum s = case s of
  PrintInt -> Imm "1"
  PrintChar -> Imm "11"
  ReadInt -> Imm "5"
  ReadChar -> Imm "12"

-- Print the integer i to standard output.
-- void puti(int i)
puti :: P.PhysicalFunction
puti = Function
  (FunctionName (Label "puti"))
  False -- No return value
  [ParamV (Variable "i")]
  [] -- No local vars
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
putc :: P.PhysicalFunction
putc = Function
  (FunctionName (Label "putc"))
  False -- No return value
  [ParamV (Variable "i")]
  [] -- No local vars
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
getc :: P.PhysicalFunction
getc = Function
  (FunctionName (Label "getc"))
  True -- No return value
  []   -- No params
  []   -- No local var
  [ P.Li SyscallCode (syscallNum ReadChar)
  , P.Syscall
  , P.Add Retval V0 ZeroReg     -- store char into retval
  , P.Jr RetAddr
  ]

-- Read an integer from standard input.
geti :: P.PhysicalFunction
geti = Function
  (FunctionName (Label "geti"))
  False -- No return value
  [] -- No params
  [] -- No local var
  [ P.Li SyscallCode (syscallNum ReadInt)
  , P.Syscall
  , P.Add Retval V0 ZeroReg -- Result stored in $v0
  , P.Jr RetAddr
  ]

-- Intrinsic func for array assign 
-- We load `val` into `arr` from index `size` - 1 to 0
-- ex) assign arr, size, val
--     args = [arr, size, val]
--
-- void memset(int *arr, int size, int val)
memset :: P.PhysicalFunction
memset = Function
  (FunctionName (Label "memset"))
  False -- No return value
  [] -- No params
  [] -- No local var
  -- TODO: Replace this with the dumb N "addi" instrs
  []
  --[ P.Lw (M M1) (Imm "-8") Fp     
  --, P.Addi (M M2) ZeroReg (Imm "4")  
  --, P.Mult (M M1) (M M2)
  --, P.Mflo (M M2)                 -- load size to M2
  --, P.Label (Label "LOOP")
  --, P.Blez (M M2) (Label "EXIT") -- while size >= 0
  --, P.Addi (M M1) ZeroReg (Imm "4")    --   size = size - 1
  --, P.Sub (M M1) (M M2) (M M1)
  --, P.Lw (M M2) (Imm "-4") Fp     
  --, P.Add (M M2) (M M1) (M M2) 
  --, P.Lw (M M1) (Imm "-12") Fp 
  --, P.Sw (M M1) (Imm "0") (M M2)       --   arr[size] = val
  --, P.Lw (M M1) (k x) Sp          
  --, P.Sub (M M2) (M M2) (M M1)
  --, P.J (Label "LOOP")
  --, P.Label (Label "EXIT") ] 

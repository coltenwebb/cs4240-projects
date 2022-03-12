-- TODO: IMPORTANT!!!! Re-enable this
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
module MIPS.Printer where

import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P
import MIPS.Types.Operand

class Print p where
  pr :: p -> String

-- ============
-- = Virtual =
-- ============

instance Print V.VirtualProgram where
  pr (V.VirtualProgram vfuncs) =
    concatMap pr vfuncs

instance Print V.VirtualFunction where
  pr (V.VirtualFunction vinsts fname) =
    concatMap f vinsts
    where
      f (V.Label (Label s)) = pr $ V.Label (Label (fname ++ "_" ++ s))
      f x         = pr x

instance Print V.MipsVirtual where
  pr (V.Addi dst src imm)
    = "    addi " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr imm  ++ "\n"
  pr (V.Add dst src1 src2)
    = "    add " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2 ++ "\n"
  pr (V.Sub dst src1 src2)
    = "    sub " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2 ++ "\n"
  pr (V.Mult dst src1 src2)
    = "    mult " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2 ++ "\n"
  pr (V.Div dst src1 src2)
    = "    div " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2 ++ "\n"
  pr (V.Andi dst src imm)
    = "    andi " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr imm  ++ "\n"
  pr (V.And dst src1 src2)
    = "    and " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2 ++ "\n"
  pr (V.Ori dst src imm)
    = "    ori " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr src  ++ "\n"
  pr (V.Or dst src1 src2)
    = "    or " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2 ++ "\n"
--  pr (V.Beq dst src label)
--    = "    beq " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr label  ++ "\n"
--  pr (V.Bne dst src label)
--    = "    bne " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr label  ++ "\n"
--  pr (V.Bgtz dst label)
--    = "    bgtz " ++ pr dst ++ ", " ++ pr label  ++ "\n"
--  pr (V.Blez dst label)
--    = "    blez " ++ pr dst ++ ", " ++ pr label  ++ "\n"
  pr (V.Lw base imm src )
    = "    lw " ++ pr src ++ ", " ++ pr imm ++ "(" ++ pr base  ++ ")\n"
  pr (V.Sw src imm base )
    = "    sw " ++ pr src ++ ", " ++ pr imm ++ "(" ++ pr base  ++ ")\n"
  pr (V.Label label)
    = pr label ++ ": \n"
  pr (V.Goto label )
    = "    goto " ++ pr label ++ "\n"
--  pr (V.Call label args)
--    = "    call " ++ pr label ++ concatMap (\reg -> ", " ++ pr reg) args ++ "\n"
--  pr (V.Callr dst label args)
--    = "    callr " ++ pr dst ++ ", " ++ pr label ++ concatMap (\reg -> ", " ++ pr reg) args ++ "\n"
--  pr (V.Return (Just ret))
--    = "    return " ++ pr ret ++ "\n"
--  pr (V.Return (Nothing))
--    = "    return\n"
  pr _ = "unimplemented"


-- ============
-- = Physical =
-- ============

instance Print P.PhysicalProgram where
  pr (P.PhysicalProgram pinsts) =
    concatMap (\ins -> pr ins ++ "\n") pinsts

instance Print P.MipsPhys where
  pr (P.Addi r1 r2 im) = "    addi " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr im
  pr (P.Add r1 r2 r3)  = "    add "  ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
  pr (P.Sub r1 r2 r3)  = "    sub "  ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
  pr (P.Mflo r1)       = "    mflo " ++ pr r1
  pr (P.Mult r1 r2)    = "    add "  ++ pr r1 ++ ", " ++ pr r2
  pr (P.Div r1 r2)     = "    div "  ++ pr r1 ++ ", " ++ pr r2
  pr (P.Andi r1 r2 im) = "    andi " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr im
  pr (P.And r1 r2 r3)  = "    and "  ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
  pr (P.Ori r1 r2 im)  = "    ori "  ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr im
  pr (P.Or r1 r2 r3)   = "    or "   ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
  pr (P.Jal lab)       = "    jal "  ++ pr lab
  pr (P.Jr r)          = "    jr "   ++ pr r
  pr (P.J lab)         = "    jr "   ++ pr lab
  pr (P.Beq r1 r2 lab) = "    beq "  ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr lab
  pr (P.Bne r1 r2 lab) = "    bne "  ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr lab
  pr (P.Bgtz r lab)    = "    bgtz " ++ pr r  ++ ", " ++ pr lab
  pr (P.Blez r lab)    = "    blez " ++ pr r  ++ ", " ++ pr lab
  pr (P.Lw r1 im r2)   = "    lw "   ++ pr r1 ++ ", " ++ pr im ++ ", " ++ pr r2
  pr (P.Sw r1 im r2)   = "    sw "   ++ pr r1 ++ ", " ++ pr im ++ ", " ++ pr r2
  pr (P.Label lab)     = pr lab ++ ":"
  pr (P.Syscall)       = "syscall"
  pr (P.Li r im)       = "    li" ++ pr r ++ pr im


-- ============
-- = Operands =
-- ============

instance Print VReg where
  pr (VReg num) = "$sym" ++ show num

instance Print PReg where
  pr ZeroReg = "$zero"
  pr RetAddr = "$ra"
  pr Retval = "$v0"
  pr SyscallCode = "$v0" -- verify this is correct
  pr Sp = "$sp"
  pr Fp = "$fp"
  pr (T tmpreg) = pr tmpreg
  pr (M mreg) = pr mreg
  pr (A argreg) = pr argreg
  pr V0 = "$v0"

instance Print TmpReg where
  pr T0 = "$t0"
  pr T1 = "$t1"
  pr T2 = "$t2"
  pr T3 = "$t3"
  pr T4 = "$t4"
  pr T5 = "$t5"
  pr T6 = "$t6"
  pr T7 = "$t7"

instance Print MReg where
  pr M1 = "$t8"
  pr M2 = "$t9"

instance Print ArgReg where
  pr A0 = "$a0"
  pr A1 = "$a1"
  pr A2 = "$a2"
  pr A3 = "$a3"

instance Print Imm where
  pr (Imm str) = str

instance Print Label where
  pr (Label str) = str

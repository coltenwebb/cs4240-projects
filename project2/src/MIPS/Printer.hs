-- TODO: IMPORTANT!!!! Re-enable this
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
module MIPS.Printer where

import qualified MIPS.Types.Virtual as V
import qualified MIPS.Types.Physical as P
import MIPS.Types.Operand
import MIPS.Intrinsics

import TigerIR.Program

import Data.List (intercalate)
import Control.Monad.Writer

class Print p where
  pr :: p -> String

-- ============
-- = Virtual =
-- ============

--instance Print V.VirtualProgram where
--  pr (V.VirtualProgram vfuncs) =
--    concatMap (\f -> pr f ++ "\n") vfuncs
--
--instance Print V.VirtualFunction where
--  pr (V.VirtualFunction vinsts (FunctionName (Label fname))) =
--    concatMap (\vinst -> pr vinst ++ "\n") vinsts

instance Print V.MipsVirtual where
  pr (V.Addi dst src imm)
    = "    addi " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr imm
  pr (V.Add dst src1 src2)
    = "    add " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2
  pr (V.Sub dst src1 src2)
    = "    sub " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2
  --pr (V.Subi dst src imm)
  --  = "    subi " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr imm
  pr (V.Mult dst src1 src2)
    = "    mult " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2
  pr (V.Multi dst src imm)
    = "    multi " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr imm
  pr (V.Div dst src1 src2)
    = "    div " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2
  --pr (V.Divi dst src1 imm)
  --  = "    div " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr imm
  pr (V.Andi dst src imm)
    = "    andi " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr imm
  pr (V.And dst src1 src2)
    = "    and " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2
  pr (V.Ori dst src imm)
    = "    ori " ++ pr dst ++ ", " ++ pr src ++ ", " ++ pr src
  pr (V.Or dst src1 src2)
    = "    or " ++ pr dst ++ ", " ++ pr src1 ++ ", " ++ pr src2
  pr (V.Bri c r1 imm label)
    = "    bri " ++ pr c ++ ", " ++ pr r1 ++ ", " ++ pr imm ++ ", " ++  pr label
  pr (V.Br c r1 r2 label)
    = "    br " ++ pr c ++ ", " ++ pr r1 ++ ", " ++ pr r2 ++ pr label
  pr (V.Lw base imm src )
    = "    lw " ++ pr src ++ ", " ++ pr imm ++ "(" ++ pr base  ++ ")"
  pr (V.Sw src imm base )
    = "    sw " ++ pr src ++ ", " ++ pr imm ++ "(" ++ pr base  ++ ")"
  pr (V.Label label)
    = pr label ++ ":"
  pr (V.Goto label )
    = "    goto " ++ pr label
  pr (V.AssignI src imm)
    = "    assigni " ++ pr src ++ ", " ++ pr imm
  pr (V.AssignV src v)
    = "    assign " ++ pr src ++ ", " ++ pr v
  pr (V.Li dst imm)
    = "    li " ++ pr dst ++ ", " ++ pr imm
  pr (V.Call label args)
   = "    call " ++ pr label ++ concatMap (\reg -> ", " ++ pr reg) args
  pr (V.Callr dst label args)
   = "    callr " ++ pr dst ++ ", " ++ pr label ++ concatMap (\reg -> ", " ++ pr reg) args
  pr (V.ArrStr src dst idx)
   = "    arrStr " ++ pr dst ++ ", " ++ pr src ++ pr idx
  pr (V.ArrStri src dst idx)
   = "    arrStri " ++ pr dst ++ ", " ++ pr src ++ pr idx
  pr (V.ArrStrii src dst idx)
   = "    arrStrii " ++ pr dst ++ ", " ++ pr src ++ pr idx
  pr (V.ArrStriv src dst idx)
   = "    arrStriv " ++ pr dst ++ ", " ++ pr src ++ pr idx
  pr (V.ArrLoad dst arr idx)
   = "    arrStr " ++ pr dst ++ ", " ++ pr arr ++ pr idx
  pr (V.ArrLoadi dst arr idx)
   = "    arrStr " ++ pr dst ++ ", " ++ pr arr ++ pr idx
  pr (V.ArrAssignII arr size val)
   = "    arrAssignII " ++ pr arr ++ ", " ++ pr size ++ ", " ++ pr val
  pr (V.ArrAssignIV arr size val)
   = "    arrAssignIV " ++ pr arr ++ ", " ++ pr size ++ ", " ++ pr val
  pr (V.ArrAssignVI arr size val)
   = "    arrAssignVI " ++ pr arr ++ ", " ++ pr size ++ ", " ++ pr val
  pr (V.ArrAssignVV arr size val)
   = "    arrAssignVV " ++ pr arr ++ ", " ++ pr size ++ ", " ++ pr val
  pr V.Nop
   = "    nop"
  pr (V.Return val)
   = "    return " ++ pr val
  pr (V.Returni val)
   = "    returni " ++ pr val
  pr V.EndFunction
   = "    endFunc"
  pr V.BeginFunction 
   = "    beginFunc"

--  pr (V.Return (Just ret))
--    = "    return " ++ pr ret 
--  pr (V.Return (Nothing))
--    = "    return\n"

instance Print V.CallArg where
  pr (V.CVarg vreg) = pr vreg
  pr (V.CIarg imm) = pr imm

-- ============
-- = Physical =
-- ============

instance Print P.PhysicalProgram where
  pr (Program funcs) = intercalate "\n" $
    [".text"]
    ++ map pr intrinsicFunctions
    ++ [".globl main"]
    ++ map pr funcs
    --concatMap (\ins -> pr ins ++ "\n") pinsts

instance Print P.PhysicalFunction where
  pr (Function (FunctionName fn@(Label fname)) _ _ _ instrs) =
    intercalate "\n" $ (fname ++ ":") : map pr' instrs
    where
      pr' :: P.MipsPhys -> String
      pr' (P.Addi r1 r2 im) = "    addi " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr im
      pr' (P.Add r1 r2 r3)  = "    add " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
      pr' (P.Sub r1 r2 r3)  = "    sub " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
      pr' (P.Mflo r1)       = "    mflo " ++ pr r1
      pr' (P.Mult r1 r2)    = "    add " ++ pr r1 ++ ", " ++ pr r2
      pr' (P.Div r1 r2)     = "    div " ++ pr r1 ++ ", " ++ pr r2
      pr' (P.Andi r1 r2 im) = "    andi " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr im
      pr' (P.And r1 r2 r3)  = "    and " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
      pr' (P.Ori r1 r2 im)  = "    ori " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr im
      pr' (P.Or r1 r2 r3)   = "    or " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr r3
      pr' (P.Jal lab)       = "    jal " ++ pr lab
      pr' (P.Jr r)          = "    jr " ++ pr r
      pr' (P.J lab)         = "    jr " ++ pr lab
      pr' (P.Beq r1 r2 lab) = "    beq " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr lab
      pr' (P.Bne r1 r2 lab) = "    bne " ++ pr r1 ++ ", " ++ pr r2 ++ ", " ++ pr lab
      pr' (P.Bgtz r lab)    = "    bgtz " ++ pr r ++ ", " ++ pr lab
      pr' (P.Blez r lab)    = "    blez " ++ pr r ++ ", " ++ pr lab
      pr' (P.Lw r1 im r2)   = "    lw " ++ pr r1 ++ ", " ++ pr im ++ ", " ++ pr r2
      pr' (P.Sw r1 im r2)   = "    sw " ++ pr r1 ++ ", " ++ pr im ++ ", " ++ pr r2
      pr' (P.Label (Label lab)) = fname ++ "_" ++ lab ++ ":"
      pr' P.Syscall         = "    syscall"
      pr' (P.Li r im)       = "    li" ++ ", " ++ pr r ++ ", " ++ pr im

instance Print V.Cmp where
  pr x = show x
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

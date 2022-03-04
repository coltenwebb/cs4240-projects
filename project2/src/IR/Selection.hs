module IR.Selection where

import IR.Function
import IR.Program
import IR.Instruction
import qualified Data.Map as M
import Data.Bits

-- notes
-- arrays will be passed as pointers, ints as values, onto the stack
-- at the start of a function we allocate space on stack for each variable
-- variables have an associated register and pointer on the stack. 
--   this can be determined directly from ir.
--   we don't care about variable size after stack allocation, each fits in a word

newtype AsmProgram = AsmProgram { asmInstructions :: [AsmInstruction] } deriving ()

newtype Reg = Reg String deriving (Show, Eq)
newtype Imm = Imm String deriving (Show, Eq)
newtype Lab = Lab String deriving (Show, Eq)

zreg = Reg "$0"
imreg = Reg "$8"

data AsmInstruction =
  Addi Reg Reg Imm |
  Add Reg Reg Reg |
  Sub Reg Reg Reg |
  Mflo Reg |
  Mult Reg Reg |
  Div Reg Reg |
  Andi Reg Reg Imm |
  And Reg Reg Reg |
  Ori Reg Reg Imm |
  Or Reg Reg Reg |
  Jal Lab |
  Lbu Reg Imm Reg |
  Sw  Reg Imm Reg |

  Placeholder Instruction
  deriving (Show)

instance Show AsmProgram where
  show prog =
    "prog:\n"
    ++ concatMap (\ins -> show ins ++ "\n") (asmInstructions prog)

addVals str1 str2 = show $ read str1 + read str2
subVals str1 str2 = show $ read str1 - read str2
mulVals str1 str2 = show $ read str1 * read str2
divVals str1 str2 = show $ (read str1 :: Integer) `quot` read str2
andVals str1 str2 = show $ (read str1 :: Integer) .&. read str2
orVals str1 str2 = show $ (read str1 :: Integer) .|. read str2

toAsm :: Program -> AsmProgram
toAsm = AsmProgram . programSelection

allocateRegs :: Function -> M.Map Variable Reg
allocateRegs (Function _ _ params vars _) = foldl comb M.empty (params ++ vars)
  where
    -- TODO: smarter allocation here? r8 is used for some immediate ops in instruction selection
    comb mp var = M.insert var (Reg ("$" ++ show (length mp + 9))) mp

programSelection :: Program -> [AsmInstruction]
programSelection prog = concatMap functionSelection (functions prog)

functionSelection :: Function -> [AsmInstruction]
functionSelection fn = concatMap sel (instrs fn)
  where
    reg = (M.!) (allocateRegs fn)

    sel :: Instruction -> [AsmInstruction]
    sel (Instruction ASSIGN [VariableOperand t, ConstantOperand (ConstantValue str) _] _) = [Addi (reg t) zreg (Imm str)]
    sel (Instruction ASSIGN [VariableOperand t, VariableOperand s] _) = [Addi (reg t) (reg s) (Imm "0")]

    sel (Instruction ADD [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ addVals str1 str2)]
    sel (Instruction ADD [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) (reg s) (Imm str2)]
    sel (Instruction ADD [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi (reg t) (reg s) (Imm str1)]
    sel (Instruction ADD [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Add (reg d) (reg s) (reg t)]

    -- no immediate op, so we use imreg as temp
    sel (Instruction SUB [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ subVals str1 str2)]
    sel (Instruction SUB [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Sub (reg t) (reg s) imreg]
    sel (Instruction SUB [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi (reg t) zreg (Imm str1), Sub (reg t) (reg t) (reg s)]
    sel (Instruction SUB [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Sub (reg d) (reg s) (reg t)]

    sel (Instruction MULT [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ mulVals str1 str2)]
    sel (Instruction MULT [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Mult (reg s) imreg, Mflo (reg t)]
    sel (Instruction MULT [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi imreg zreg (Imm str1), Mult imreg (reg s), Mflo (reg t)]
    sel (Instruction MULT [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Mult (reg s) (reg t), Mflo (reg d)]

    -- div similar to mult
    sel (Instruction DIV [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ divVals str1 str2)]
    sel (Instruction DIV [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Div (reg s) imreg, Mflo (reg t)]
    sel (Instruction DIV [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi imreg zreg (Imm str1), Div imreg (reg s), Mflo (reg t)]
    sel (Instruction DIV [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Div (reg s) (reg t), Mflo (reg d)]

    -- and similar to add
    sel (Instruction AND [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ andVals str1 str2)]
    sel (Instruction AND [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Andi (reg t) (reg s) (Imm str2)]
    sel (Instruction AND [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Andi (reg t) (reg s) (Imm str1)]
    sel (Instruction AND [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [And (reg d) (reg s) (reg t)]

    -- or similar to and
    sel (Instruction OR [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ orVals str1 str2)]
    sel (Instruction OR [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Ori (reg t) (reg s) (Imm str2)]
    sel (Instruction OR [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Ori (reg t) (reg s) (Imm str1)]
    sel (Instruction OR [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Or (reg d) (reg s) (reg t)]


    -- array load
    sel (Instruction ARRAY_LOAD [VariableOperand t, VariableOperand a, ConstantOperand (ConstantValue o) _] _) =
      [Lbu (reg t) (Imm $ mulVals o "4") (reg a)]
    sel (Instruction ARRAY_LOAD [VariableOperand t, VariableOperand a, VariableOperand o] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm "4"),
        Mult imreg (reg o),
        Mflo (reg o),
        Add (reg o) (reg o) (reg a),
        Lbu (reg t) (Imm "0") (reg o)]

    -- array store
    sel (Instruction ARRAY_STORE [VariableOperand s, VariableOperand a, ConstantOperand (ConstantValue o) _] _) =
      [Sw (reg s) (Imm $ mulVals o "4") (reg a)]
    sel (Instruction ARRAY_STORE [VariableOperand s, VariableOperand a, VariableOperand o] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm "4"),
        Mult imreg (reg o),
        Mflo (reg o),
        Add (reg o) (reg a) (reg o),
        Sw (reg s) (Imm "0") (reg o)]
    sel (Instruction ARRAY_STORE [ConstantOperand (ConstantValue s) _, VariableOperand a, VariableOperand o] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm "4"),
        Mult (reg o) imreg,
        Mflo imreg,
        Add (reg a) (reg a) imreg,
        And imreg zreg imreg,
        Addi imreg imreg (Imm s),
        Sw imreg (Imm "o") (reg a)]
    sel (Instruction ARRAY_STORE [ConstantOperand (ConstantValue s) _, VariableOperand a, ConstantOperand (ConstantValue o) _] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm s),
        Sw imreg (Imm (mulVals o "4")) (reg a)]

    sel (Instruction GOTO [LabelOperand (LabelName str)] _) = [Jal (Lab str)]

    sel inst = [Placeholder inst]

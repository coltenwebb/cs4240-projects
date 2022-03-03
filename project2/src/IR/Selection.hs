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
  Beq Reg Reg Lab |
  Bne Reg Reg Lab |
  Bgtz Reg Lab |
  Blez Reg Lab |

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

constValToImm (ConstantValue str) = Imm str
constValToNegatedImm (ConstantValue str) = Imm $ show (0 - (read str) :: Integer)

toAsm :: Program -> AsmProgram
toAsm = AsmProgram . programSelection

allocateRegs :: Function -> (M.Map Variable Reg)
allocateRegs (Function _ _ params vars _) = foldl comb M.empty (params ++ vars)
  where
    -- TODO: smarter allocation here? r8 is used for some immediate ops in instruction selection
    comb mp var = M.insert var (Reg ("$" ++ show (length mp + 9))) mp

programSelection :: Program -> [AsmInstruction]
programSelection prog = concat $ map functionSelection $ functions prog

functionSelection :: Function -> [AsmInstruction]
functionSelection fn = concat $ map sel $ instrs fn
  where
    reg = (M.!) (allocateRegs fn)

    sel :: Instruction -> [AsmInstruction]
    sel (Instruction ASSIGN [(VariableOperand t), (ConstantOperand (ConstantValue str) _)] _) = [Addi (reg t) zreg (Imm str)]
    sel (Instruction ASSIGN [(VariableOperand t), (VariableOperand s)] _) = [Addi (reg t) (reg s) (Imm "0")]

    sel (Instruction ADD [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) zreg (Imm $ addVals str1 str2)]
    sel (Instruction ADD [(VariableOperand t), (VariableOperand s), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) (reg s) (Imm str2)]
    sel (Instruction ADD [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (VariableOperand s)] _) = 
      [Addi (reg t) (reg s) (Imm str1)]
    sel (Instruction ADD [(VariableOperand d), (VariableOperand s), (VariableOperand t)] _) = 
      [Add (reg d) (reg s) (reg t)]

    -- no immediate op, so we use imreg as temp
    sel (Instruction SUB [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) zreg (Imm $ subVals str1 str2)]
    sel (Instruction SUB [(VariableOperand t), (VariableOperand s), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (imreg) zreg (Imm str2), Sub (reg t) (reg s) (imreg)]
    sel (Instruction SUB [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (VariableOperand s)] _) = 
      [Addi (reg t) zreg (Imm str1), Sub (reg t) (reg t) (reg s)]
    sel (Instruction SUB [(VariableOperand d), (VariableOperand s), (VariableOperand t)] _) = 
      [Sub (reg d) (reg s) (reg t)]

    sel (Instruction MULT [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) zreg (Imm $ mulVals str1 str2)]
    sel (Instruction MULT [(VariableOperand t), (VariableOperand s), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (imreg) zreg (Imm str2), Mult (reg s) (imreg), Mflo (reg t)]
    sel (Instruction MULT [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (VariableOperand s)] _) = 
      [Addi (imreg) zreg (Imm str1), Mult (imreg) (reg s), Mflo (reg t)]
    sel (Instruction MULT [(VariableOperand d), (VariableOperand s), (VariableOperand t)] _) = 
      [Mult (reg s) (reg t), Mflo (reg d)]

    -- div similar to mult
    sel (Instruction DIV [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) zreg (Imm $ divVals str1 str2)]
    sel (Instruction DIV [(VariableOperand t), (VariableOperand s), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (imreg) zreg (Imm str2), Div (reg s) (imreg), Mflo (reg t)]
    sel (Instruction DIV [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (VariableOperand s)] _) = 
      [Addi (imreg) zreg (Imm str1), Div (imreg) (reg s), Mflo (reg t)]
    sel (Instruction DIV [(VariableOperand d), (VariableOperand s), (VariableOperand t)] _) = 
      [Div (reg s) (reg t), Mflo (reg d)]

    -- and similar to add
    sel (Instruction AND [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) zreg (Imm $ andVals str1 str2)]
    sel (Instruction AND [(VariableOperand t), (VariableOperand s), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Andi (reg t) (reg s) (Imm str2)]
    sel (Instruction AND [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (VariableOperand s)] _) = 
      [Andi (reg t) (reg s) (Imm str1)]
    sel (Instruction AND [(VariableOperand d), (VariableOperand s), (VariableOperand t)] _) = 
      [And (reg d) (reg s) (reg t)]

    -- or similar to and
    sel (Instruction OR [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (reg t) zreg (Imm $ orVals str1 str2)]
    sel (Instruction OR [(VariableOperand t), (VariableOperand s), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Ori (reg t) (reg s) (Imm str2)]
    sel (Instruction OR [(VariableOperand t), (ConstantOperand (ConstantValue str1) _), (VariableOperand s)] _) = 
      [Ori (reg t) (reg s) (Imm str1)]
    sel (Instruction OR [(VariableOperand d), (VariableOperand s), (VariableOperand t)] _) = 
      [Or (reg d) (reg s) (reg t)]

    sel (Instruction GOTO [(LabelOperand (LabelName str))] _) = [Jal (Lab str)]

    sel (Instruction BREQ [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _)
      | (read str1::Integer) == read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BREQ [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (VariableOperand z)] _) = 
      [Addi (imreg) zreg (Imm str1), Beq (imreg) (reg z) (Lab str)]
    sel (Instruction BREQ [(LabelOperand (LabelName str)), (VariableOperand y), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (imreg) zreg (Imm str2), Beq (reg y) (imreg) (Lab str)]
    sel (Instruction BREQ [(LabelOperand (LabelName str)), (VariableOperand y), (VariableOperand z)] _) = 
      [Beq (reg y) (reg z) (Lab str)]

    sel (Instruction BRNEQ [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _)
      | (read str1::Integer) /= read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRNEQ [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (VariableOperand z)] _) = 
      [Addi (imreg) zreg (Imm str1), Bne (imreg) (reg z) (Lab str)]
    sel (Instruction BRNEQ [(LabelOperand (LabelName str)), (VariableOperand y), (ConstantOperand (ConstantValue str2) _)] _) = 
      [Addi (imreg) zreg (Imm str2), Bne (reg y) (imreg) (Lab str)]
    sel (Instruction BRNEQ [(LabelOperand (LabelName str)), (VariableOperand y), (VariableOperand z)] _) = 
      [Bne (reg y) (reg z) (Lab str)]

    sel (Instruction BRLT [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _)
      | (read str1::Integer) < read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRLT [(LabelOperand (LabelName str)), (ConstantOperand cv1 _), (VariableOperand z)] _) = 
      [Addi (imreg) (reg z) (constValToNegatedImm cv1), Bgtz (imreg) (Lab str)]
    sel (Instruction BRLT [(LabelOperand (LabelName str)), (VariableOperand y), (ConstantOperand cv2 _)] _) = 
      [Addi (imreg) (reg y) (constValToNegatedImm cv2), Sub imreg zreg imreg, Bgtz (imreg) (Lab str)]
    sel (Instruction BRLT [(LabelOperand (LabelName str)), (VariableOperand y), (VariableOperand z)] _) = 
      [Sub imreg (reg y) (reg z), Bgtz imreg (Lab str)]

    sel (Instruction BRGT [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _)
      | (read str1::Integer) > read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRGT [(LabelOperand (LabelName str)), (ConstantOperand cv1 _), (VariableOperand z)] _) = 
      [Addi (imreg) (reg z) (constValToNegatedImm cv1), Sub imreg zreg imreg, Bgtz (imreg) (Lab str)]
    sel (Instruction BRGT [(LabelOperand (LabelName str)), (VariableOperand y), (ConstantOperand cv2 _)] _) = 
      [Addi (imreg) (reg y) (constValToNegatedImm cv2), Bgtz (imreg) (Lab str)]
    sel (Instruction BRGT [(LabelOperand (LabelName str)), (VariableOperand y), (VariableOperand z)] _) = 
      [Sub imreg (reg z) (reg y), Bgtz imreg (Lab str)]

    sel (Instruction BRGEQ [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _)
      | (read str1::Integer) >= read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRGEQ [(LabelOperand (LabelName str)), (ConstantOperand cv1 _), (VariableOperand z)] _) = 
      [Addi (imreg) (reg z) (constValToNegatedImm cv1), Blez (imreg) (Lab str)]
    sel (Instruction BRGEQ [(LabelOperand (LabelName str)), (VariableOperand y), (ConstantOperand cv2 _)] _) = 
      [Addi (imreg) (reg y) (constValToNegatedImm cv2), Sub imreg zreg imreg, Blez (imreg) (Lab str)]
    sel (Instruction BRGEQ [(LabelOperand (LabelName str)), (VariableOperand y), (VariableOperand z)] _) = 
      [Sub imreg (reg z) (reg y), Blez imreg (Lab str)]

    sel (Instruction BRLEQ [(LabelOperand (LabelName str)), (ConstantOperand (ConstantValue str1) _), (ConstantOperand (ConstantValue str2) _)] _)
      | (read str1::Integer) <= read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRLEQ [(LabelOperand (LabelName str)), (ConstantOperand cv1 _), (VariableOperand z)] _) = 
      [Addi (imreg) (reg z) (constValToNegatedImm cv1), Sub imreg zreg imreg, Blez (imreg) (Lab str)]
    sel (Instruction BRLEQ [(LabelOperand (LabelName str)), (VariableOperand y), (ConstantOperand cv2 _)] _) = 
      [Addi (imreg) (reg y) (constValToNegatedImm cv2), Blez (imreg) (Lab str)]
    sel (Instruction BRLEQ [(LabelOperand (LabelName str)), (VariableOperand y), (VariableOperand z)] _) = 
      [Sub imreg (reg y) (reg z), Blez imreg (Lab str)]

    sel inst = [Placeholder inst] 

module IR.Selection where

import IR.Function
import IR.Program
import IR.Instruction
import qualified Data.Map as M

-- notes
-- arrays will be passed as pointers, ints as values, onto the stack
-- at the start of a function we allocate space on stack for each variable
-- variables have an associated register and pointer on the stack. 
--   this can be determined directly from ir.
--   we don't care about variable size after stack allocation, each fits in a word

newtype AsmProgram = AsmProgram { asmInstructions :: [AsmInstruction] } deriving Show

newtype Reg = Reg String deriving (Show, Eq)
newtype Imm = Imm String deriving (Show, Eq)

data AsmInstruction = 
  Addi Reg Reg Imm |
  Add Reg Reg Reg
  deriving (Show)



toAsm :: Program -> AsmProgram
toAsm = undefined

allocateRegs :: Function -> (M.Map Variable Reg)
allocateRegs (Function _ _ params vars _) = foldl comb M.empty (params ++ vars)
  where
    comb mp var = M.insert var (Reg ("$" ++ show (length mp + 8))) mp

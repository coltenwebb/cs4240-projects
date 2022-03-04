{-# LANGUAGE GADTs #-}
module TigerIR.IrInstruction where

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq)

data IrInstruction = IrInstruction
  { instruction :: Instruction
  , lineNum     :: LineNumber
  }

data Instruction
  = Assign AssignOperands
  | Add    BinOperands

data AssignOperands a where
    VarAssign :: a -> AssignOperands a

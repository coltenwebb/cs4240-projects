module IR.Function where

import IR.Instruction
import IR.Type

data Function = Function
  { name :: String
  , returnType :: Type
  , parameters :: [VarOperand]
  , variables :: [VarOperand]
  , instruction :: [Instruction]
  }

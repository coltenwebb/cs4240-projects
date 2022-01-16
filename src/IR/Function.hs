module IR.Function where

import IR.Instruction
import IR.Type

data Function = Function
  { name :: FunctionName
  , returnType :: Type
  , parameters :: [Variable]
  , variables :: [Variable]
  , instruction :: [Instruction]
  } deriving Show

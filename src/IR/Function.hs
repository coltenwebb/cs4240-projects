module IR.Function where

import IR.Instruction
import IR.Type

data Function = Function
  { name :: FunctionName
  , returnType :: Type
  , parameters :: [Variable]
  , variables :: [Variable]
  , instruction :: [Instruction]
  }

instance Show Function where
  show(Function fn rt ps vs is) =
    "#start_function\n"
    ++ show rt ++ " " ++ show fn
    ++ "(" ++ show ps ++ "):\n"
    ++ concatMap (\ins -> "    " ++ show ins ++ "\n") is
    ++ "#end_function"

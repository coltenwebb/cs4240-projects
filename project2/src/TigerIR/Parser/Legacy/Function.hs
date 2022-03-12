module TigerIR.Parser.Legacy.Function where

import TigerIR.Parser.Legacy.Instruction
import TigerIR.Parser.Legacy.Type

import qualified Data.Map as M
import qualified Data.List as L


data Function = Function
  { name :: FunctionName
  , returnType :: Type
  , parameters :: [Variable]
  , variables :: [Variable]
  , instrs :: [Instruction]
  }

instance Show Function where
  show(Function fn rt ps vs is) =
    "#start_function\n"
    ++ show rt ++ " " ++ show fn
    ++ "(" ++ show ps ++ "):\n"
    ++ concatMap (\ins -> "    " ++ show ins ++ "\n") is
    ++ "#end_function"

-- lineNumToInst :: Function -> M.Map LineNumber Instruction 
getInstructionByLineNum :: Function -> LineNumber -> Instruction
getInstructionByLineNum fn num = 
  case L.find (\i -> lineNum i == num) (instrs fn) of
    Nothing -> error $ "Cannot find valid Instruction for line number " ++ show num
    Just i -> i 

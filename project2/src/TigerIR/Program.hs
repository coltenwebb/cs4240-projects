{-# LANGUAGE FlexibleInstances #-}
module TigerIR.Program where

import TigerIR.Types as T
import TigerIR.IrInstruction
import Data.List

type TigerIrFunction = Function TigerIrIns
type TigerIrProgram  = Program  TigerIrIns

data Function a = Function
  { name       :: FunctionName
  -- Tiger-IR.pdf:
  -- Notes on semantics: The return value of a function cannot be array-typed.
  , returnsVal :: Bool -- int or void
  , parameters :: [ParamVar]
  , localVars  :: [LocalVar]
  , instrs     :: [a]
  }

newtype Program a = Program { functions :: [Function a] }

instance Show TigerIrFunction where
  show fn@(Function fname rval params lvars instrs)
    = (if rval then "int " else "void ") ++ show fname ++ "(" ++ intercalate ", " (map show params) ++ "):\n"
      ++ "int-list: " ++ intercalate ", " (map show lvars) ++ "\n"
      ++ concatMap (\inst -> show inst ++ "\n") instrs


instance Show TigerIrProgram where 
  show (Program funcs) = concatMap (\f -> show f ++ "\n") funcs

instrSelectionPass :: (a -> b) -> Function a -> Function b
instrSelectionPass f fn = fn { instrs = map f (instrs fn) } 

instrSelectionPassFlatten :: (a -> [b]) -> Function a -> Function b
instrSelectionPassFlatten f fn = fn { instrs = concatMap f (instrs fn) }

programSelectionPass :: (Function a -> Function b) -> Program a -> Program b
programSelectionPass f (Program fns) = Program (map f fns)
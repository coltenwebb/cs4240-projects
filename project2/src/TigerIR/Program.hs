module TigerIR.Program where

import TigerIR.Types
import TigerIR.IrInstruction
import Data.List
data TigerIrFunction = TigerIrFunction
  { name       :: FunctionName
  -- Tiger-IR.pdf:
  -- Notes on semantics: The return value of a function cannot be array-typed.
  , returnsVal :: Bool -- int or void
  , parameters :: [ParamVar]
  , localVars  :: [LocalVar]
  , instrs     :: [TigerIrIns]
  }
instance Show TigerIrFunction where
  show fn@(TigerIrFunction fname rval params lvars instrs)
    = (if rval then "int " else "void ") ++ show fname ++ "(" ++ intercalate ", " (map show params) ++ "):\n"
      ++ "int-list: " ++ intercalate ", " (map show lvars) ++ "\n"
      ++ concatMap (\inst -> show inst ++ "\n") instrs

newtype TigerIrProgram = TigerIrProgram { functions :: [TigerIrFunction] }

instance Show TigerIrProgram where 
  show (TigerIrProgram funcs) = concatMap (\f -> show f ++ "\n") funcs
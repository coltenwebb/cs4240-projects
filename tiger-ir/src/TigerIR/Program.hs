module TigerIR.Program where

import TigerIR.Types
import TigerIR.IrInstruction

data TigerIrFunction = TigerIrFunction
  { name       :: FunctionName
  -- Tiger-IR.pdf:
  -- Notes on semantics: The return value of a function cannot be array-typed.
  , returnType :: VarType
  , parameters :: Parameters
  , localVars  :: LocalVars
  , instrs     :: IrInstruction
  }

newtype TigerIrProgram = TigerIrProgram { functions :: [TigerIrFunction] }
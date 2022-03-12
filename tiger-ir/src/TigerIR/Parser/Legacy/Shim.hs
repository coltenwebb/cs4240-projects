module TigerIR.Parser.Legacy.Shim where

import TigerIR.Parser.Legacy.Function as L
import TigerIR.Parser.Legacy.Type as L
import TigerIR.Parser.Legacy.Instruction as L

import TigerIR.Types as T

legacyParams2NewParams :: [L.Variable] -> T.Parameters
legacyParams2NewParams = map f
  where
    f (L.Variable (L.VariableName vname) tp) = case tp of
      L.IntType -> T.ParamV (T.Variable vname)

      L.ArrayType (L.ArraySize sz) L.IntType
        -> T.ParamA (T.Array (T.Variable vname) (T.ArraySize sz))

      _ -> error $ "Unsupported fn param type "
        ++ show tp ++ " for variable `"
        ++ vname
        ++ "`, note no float support for proj. 2"

legacyCallArgs2FnArgs :: [L.Operand] -> T.FnArgs
legacyCallArgs2FnArgs = map f
  where
    f oprnd = case oprnd of
      L.VariableOperand (L.Variable (L.VariableName vname) tp) -> case tp of
        L.IntType -> T.Varg (T.Variable vname)

        L.ArrayType (L.ArraySize sz) L.IntType
          -> T.Aarg (T.Array (T.Variable vname) (T.ArraySize sz))
        
        L.ArrayType (L.ArraySize sz) _
          -> error "non-int arrays unsupported"

        L.FloatType -> error "float type not supported fn arg"

        L.VoidType -> error "void type not supported fn arg"
      
      L.ConstantOperand (ConstantValue c) L.IntType
        -> T.Iarg (Imm c)

      L.ConstantOperand _ _ -> error "non-int constants unsupported"
      
      L.FunctionOperand _ -> error "function unsupported fn arg type"

      L.LabelOperand _ -> error "label unsupported fn arg type"
      
      

v2v :: L.Variable -> T.Variable
v2v (L.Variable (VariableName vn) tp) = case tp of
  L.IntType -> T.Variable vn
  _ -> error $ "Used v2v wrong, use a2a to convert old array to new arrays"
    ++ " unsupported variable type " ++ show tp ++ " in v2v"

c2i :: L.ConstantValue -> T.Imm
c2i (ConstantValue str) = T.Imm str

a2a :: L.Variable -> T.Array
a2a (L.Variable (VariableName vn) tp) = case tp of
  ArrayType (L.ArraySize sz) L.IntType ->  T.Array (T.Variable vn) (T.ArraySize sz)
  _ -> error $ "unsupported variable type " ++ show tp ++ " in a2a"
    ++ " used a2a wrong, use v2v to convert old variables to new variables"
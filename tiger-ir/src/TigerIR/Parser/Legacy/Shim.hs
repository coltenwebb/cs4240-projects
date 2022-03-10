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
        ++ vname ++ "`"





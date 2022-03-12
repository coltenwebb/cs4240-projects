module TigerIR.Parser.Legacy.Type where

-- Note, originally Integer, changed to
-- Int to make it easier for shim
newtype ArraySize = ArraySize Int
  deriving (Eq, Ord, Show)

data Type =
  IntType
  | FloatType
  | ArrayType { size :: ArraySize,  elemType :: Type }
  | VoidType
  deriving (Eq, Ord, Show)

isFloatType t = case t of
  FloatType -> True
  (ArrayType _ FloatType) -> True
  otherwise -> False

isIntType t = case t of
  IntType -> True
  (ArrayType _ IntType) -> True
  otherwise -> False

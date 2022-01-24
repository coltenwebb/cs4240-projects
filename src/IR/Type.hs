module IR.Type where

newtype ArraySize = ArraySize Integer
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

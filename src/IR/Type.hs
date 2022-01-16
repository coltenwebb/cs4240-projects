module IR.Type where

newtype ArraySize = ArraySize Integer
  deriving (Eq, Ord, Show)

data Type =
  IntType
  | FloatType
  | ArrayType { size :: ArraySize,  elemType :: Type }
  | VoidType
  deriving (Eq, Ord, Show)

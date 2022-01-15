module IR.Type where

newtype ArraySize = ArraySize Int

data Type =
  IntType
  | FloatType
  | ArrayType { size :: ArraySize,  elemType :: Type }
  | VoidType

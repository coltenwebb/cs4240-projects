module TigerIR.VarType where

import Data.Array (Array)
newtype ArraySize = ArraySize Integer
  deriving (Eq, Ord, Show)

data VarType
  = IntType
  | FloatType
  | IntArrayType ArraySize
  | FloatArrayType ArraySize
  | VoidType
  deriving (Eq, Ord, Show)
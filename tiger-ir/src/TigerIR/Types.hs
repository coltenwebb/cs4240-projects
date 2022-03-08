{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module TigerIR.Types where
newtype ArraySize = ArraySize Integer
  deriving (Eq, Ord, Show)

data VarType
  = IntType
  | FloatType
  | VoidType
  deriving (Eq, Ord, Show)

data ArrType
  = IntArr
  | FloatArr

newtype ConstantValue = ConstantValue String deriving (Eq, Ord, Show)
newtype Constant (a :: VarType) = Constant ConstantValue deriving (Eq, Ord, Show)

newtype VariableName = VariableName String deriving (Eq, Ord, Show)
newtype Variable (a :: VarType) = Variable VariableName deriving (Eq, Ord, Show)

newtype ArrayName = ArrayName String deriving (Eq, Ord, Show)
data Array (a :: ArrType) = Array ArrayName ArraySize

newtype Label = Label String deriving (Eq, Ord, Show)
newtype FunctionName = FunctionName String deriving (Eq, Ord, Show)
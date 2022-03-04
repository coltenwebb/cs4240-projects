module TigerIR.Variable where

import TigerIR.Type

newtype VariableName = VariableName String deriving (Eq, Ord, Show)

data Variable = Variable VariableName Type
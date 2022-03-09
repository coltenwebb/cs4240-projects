module TigerIR.Types where

newtype ArraySize = ArraySize Int
  deriving (Eq, Ord, Show)


-- Immediate Values and Labels
newtype Imm   = Imm     String             deriving (Show, Eq, Ord)
newtype Label = Label   String             deriving (Show, Eq, Ord)

newtype Variable = Variable String deriving (Eq, Ord, Show)
data Array = Array Variable ArraySize deriving (Eq, Ord, Show)

newtype FunctionName = FunctionName Label deriving (Eq, Ord, Show)

-- Parameters and local vars can't be 
newtype Parameters = Parameters [InitVar]
newtype LocalVars  = LocalVars  [InitVar]
newtype FnArgs     = FnArgs     [FnArg]

data InitVar
  = InitV Variable
  | InitA Array
  deriving (Ord, Eq, Show)

data FnArg
  = Varg Variable
  | Aarg Array
  | Iarg Imm
  deriving (Ord, Eq, Show)

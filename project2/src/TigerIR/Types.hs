module TigerIR.Types where

newtype ArraySize = ArraySize Int
  deriving (Eq, Ord, Show)

data Type =
  IntType
  | VoidType
  deriving (Eq, Ord, Show)

-- Immediate Values and Labels
newtype Imm   = Imm     String             deriving (Show, Eq, Ord)
newtype Label = Label   String             deriving (Show, Eq, Ord)

newtype Variable = Variable String deriving (Eq, Ord, Show)
data Array = Array Variable ArraySize deriving (Eq, Ord, Show)

newtype FunctionName = FunctionName Label deriving (Eq, Ord, Show)

-- Parameters and local vars can't be 
type Parameters = [ParamVar]
type LocalVars  = [LocalVar]
type FnArgs     = [FnArg]

data ParamVar
  = ParamV Variable
  | ParamA Array
  deriving (Ord, Eq, Show)

data LocalVar
  = LocalV Variable
  | LocalA Array
  deriving (Ord, Eq, Show)

-- For Virtual MIPS to know whether we need
-- extra registers or if we are passing
-- an imm val as arg
data FnArg
  = Varg Variable
  | Aarg Array
  | Iarg Imm
  deriving (Ord, Eq, Show)

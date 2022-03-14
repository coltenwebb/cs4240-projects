module TigerIR.Types where

newtype ArraySize = ArraySize Int
  deriving (Eq, Ord)
instance Show ArraySize where 
  show (ArraySize i) = show i

data Type =
  IntType
  | VoidType
  deriving (Eq, Ord, Show)

-- Immediate Values and Labels
newtype Imm   = Imm     String             deriving (Eq, Ord)
instance Show Imm where 
  show (Imm l) = l
newtype Label = Label   String             deriving (Eq, Ord)
instance Show Label where 
  show (Label l) = l

newtype Variable = Variable String deriving (Eq, Ord)
instance Show Variable where 
  show (Variable vname) = vname 
data Array = Array Variable ArraySize deriving (Eq, Ord)
instance Show Array where 
  show (Array v size) = show v ++ "[" ++ show size ++ "]"

newtype FunctionName = FunctionName Label deriving (Eq, Ord)
instance Show FunctionName where 
  show (FunctionName (Label name)) = name

-- Parameters and local vars can't be 
type Parameters = [ParamVar]
type LocalVars  = [LocalVar]
type FnArgs     = [FnArg]

data ParamVar
  = ParamV Variable
  | ParamA Array
  deriving (Ord, Eq)
instance Show ParamVar where 
  show (ParamV v) = show v 
  show (ParamA a) = show a

data LocalVar
  = LocalV Variable
  | LocalA Array
  deriving (Ord, Eq)
instance Show LocalVar where 
  show (LocalV (Variable vname)) = vname
  show (LocalA (Array (Variable vname) (ArraySize size))) = vname ++ "[" ++ show size ++ "]"

-- For Virtual MIPS to know whether we need
-- extra registers or if we are passing
-- an imm val as arg
data FnArg
  = Varg Variable
  | Aarg Array
  | Iarg Imm
  deriving (Ord, Eq)
instance Show FnArg where 
  show (Varg v) = show v 
  show (Aarg a) = show a 
  show (Iarg i) = show i

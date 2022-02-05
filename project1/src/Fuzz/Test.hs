{-# LANGUAGE DerivingVia #-}

module Fuzz.Test where

import qualified Test.QuickCheck as Q
import IR.Instruction
import IR.Function
import IR.Type
import Data.List as L (foldl', reverse, head, sort)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Data.Maybe (mapMaybe)
import IR.Optimizer.CFG
import Debug.Trace
import IR.Printer
import IR.Type (Type(ArrayType, FloatType))

data Tree = Leaf Int | Node Tree Int Tree deriving (Eq, Show)

instance Q.Arbitrary Tree where
  arbitrary = Q.sized go
    where go 0 = Leaf <$> Q.arbitrary
          go n = Q.oneof [Leaf <$> Q.arbitrary, Node <$> go' <*> Q.arbitrary <*> go']
            where go' = go (n-1)

instance Q.Arbitrary Type where
  arbitrary = arbArrayType >>= \arrType -> Q.elements [IntType, FloatType, VoidType, arrType]
    where
      arbArrayType = ArrayType <$> arbArraySize <*> (Q.arbitrary :: Q.Gen Type)
      arbArraySize = ArraySize <$> (Q.arbitrary :: Q.Gen Integer)

instance Q.Arbitrary Variable where
  arbitrary = Variable <$> (VariableName <$> identifier) <*> randVariableType
    where
      randVariableType = randArrayType >>= \arrType -> Q.elements [IntType, FloatType, arrType]
      randArrayType = ArrayType <$> randArraySize <*> Q.elements [IntType, FloatType]
      randArraySize = ArraySize <$> ((Q.arbitrary :: Q.Gen Integer) `Q.suchThat` (> 0))

identifier :: Q.Gen String
identifier = (:) <$> randNonNumeral <*> Q.oneof
  [ return ""
  , (:) <$> randNumeral <*> identifier
  , (:) <$> randNonNumeral <*> identifier
  ]
  where
    randNonNumeral = Q.oneof [Q.choose ('a','z'), Q.choose ('A','Z'), return '_']
    randNumeral = Q.choose ('0', '9')

data GeneratedVars = GeneratedVars {
  intVars :: [Variable]
, floatVars :: [Variable]
, intArrayVars :: [Variable]
, floatArrayVars :: [Variable]
, labels :: [Operand]
}

genBinOpInst :: GeneratedVars -> Q.Gen Instruction
genBinOpInst (GeneratedVars intVars floatVars _ _ _) = do
  opcode' <- Q.elements [ADD, SUB, MULT, DIV, AND, OR]
  intOperation <- Q.arbitrary :: Q.Gen Bool
  intOperands <- replicateM 3 (VariableOperand <$> Q.elements intVars)
  floatOperands <- replicateM 3 (VariableOperand <$> Q.elements floatVars)
  lineNum <- LineNumber <$> randPositiveInt
  let operands' = if intOperation then intOperands else floatOperands
  return (Instruction opcode' operands' lineNum)

genBranchInst :: GeneratedVars -> Q.Gen Instruction
genBranchInst (GeneratedVars intVars floatVars _ _ labels) = do
  opcode' <- Q.elements [BREQ, BRNEQ, BRLT, BRGT, BRLEQ, BRGEQ]
  label' <- Q.elements labels
  compareInt <- Q.arbitrary :: Q.Gen Bool
  intCompare <- replicateM 2 (VariableOperand <$> Q.elements intVars)
  floatCompare <- replicateM 2 (VariableOperand <$> Q.elements floatVars)
  let operands' = label' : (if compareInt then intCompare else floatCompare)
  lineNum <- LineNumber <$> randPositiveInt
  return (Instruction opcode' operands' lineNum)

genGotoInst :: GeneratedVars -> Q.Gen Instruction
genGotoInst (GeneratedVars _ _ _ _ labels)= do
  label' <- Q.elements labels
  lineNum <- LineNumber <$> randPositiveInt
  return (Instruction GOTO [label'] lineNum)

genInst :: GeneratedVars -> Q.Gen Instruction
genInst gv = do
  let binOpInst   = genBinOpInst gv
  let branchInst  = genBranchInst gv
  let gotoInst    = genGotoInst gv
  Q.frequency [(4, binOpInst), (1, branchInst), (1, gotoInst)]

randPositiveInt :: Q.Gen Int
randPositiveInt = (Q.arbitrary :: Q.Gen Int) `Q.suchThat` (> 0)

genLabels :: Int -> Q.Gen [Operand]
genLabels n = replicateM n $ LabelOperand . LabelName <$> identifier

insertRandomLabels :: GeneratedVars -> [Instruction] -> Q.Gen [Instruction]
insertRandomLabels gv insts = do
  linenumbers <- sort <$> (replicateM c $ (Q.arbitrary :: Q.Gen Int) `Q.suchThat` (\i -> i > 0 && i <= k))
  let labels'' = zip linenumbers labels'
  let (xs, _, res) = foldl' f (labels'', 0, []) insts
  return $ res ++ map (\(_, operand) -> Instruction LABEL [operand] (LineNumber (-1))) xs
  where
    f :: ([(Int, Operand)], Int, [Instruction]) -> Instruction -> ([(Int, Operand)], Int, [Instruction])
    f ([], c, is) i = ([], c + 1, is ++ [i])
    f (x:xs, c, is) i = if c == fst x then (xs, c, is ++ [i, Instruction LABEL [snd x] (LineNumber c)]) else (x:xs, c + 1, is ++ [i])

    labels' = labels gv
    k = length insts
    c = length labels'

-- LineNumber doesn't matter since we will be transforming instructions to IR
genRandomFunc :: Q.Gen [Instruction]
genRandomFunc = do
  varCount <- randPositiveInt
  labelCount <- (Q.arbitrary :: Q.Gen Int) `Q.suchThat` (\i -> i > 0 && i < 10)
  variables <- replicateM varCount (Q.arbitrary :: Q.Gen Variable)
  labels <- genLabels 5
  instCount <- randPositiveInt

  let variables'    = int1 : float1 : intArr1 : floatArr1 : variables
      generatedVars = GeneratedVars intVars floatVars intArrVars floatArrVars labels
      intVars       = filter (\v -> variableType v == IntType) variables'
      floatVars     = filter (\v -> variableType v == FloatType) variables'
      intArrVars    = filter (\v -> elemType (variableType v) == IntType) variables'
      floatArrVars  = filter (\v -> elemType (variableType v) == FloatType) variables'

  ins <- replicateM instCount $ genInst generatedVars
  insertRandomLabels generatedVars ins
  where
    int1 = Variable (VariableName "etuhteuh") IntType
    float1 = Variable (VariableName "tnoehtnuh") FloatType
    intArr1 = Variable (VariableName "") (ArrayType (ArraySize 1) IntType)
    floatArr1 = Variable (VariableName "") (ArrayType (ArraySize 1) FloatType)




-- 1. insert labels
-- 2. constant varaibles
-- 3. assign operations



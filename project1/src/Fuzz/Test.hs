{-# LANGUAGE DerivingVia #-}

module Fuzz.Test where

import Control.Monad
import Data.List as L (foldl', head, reverse, sort, nub)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace
import IR.Function
import IR.Instruction
import IR.Optimizer.CFG
import IR.Printer
import IR.Type
import qualified Test.QuickCheck as Q

data Tree = Leaf Int | Node Tree Int Tree deriving (Eq, Show)

instance Q.Arbitrary Tree where
  arbitrary = Q.sized go
    where
      go 0 = Leaf <$> Q.arbitrary
      go n = Q.oneof [Leaf <$> Q.arbitrary, Node <$> go' <*> Q.arbitrary <*> go']
        where
          go' = go (n -1)

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

randomInt :: Q.Gen Int
randomInt = Q.resize 2147483647 (Q.arbitrary :: Q.Gen Int)

randomIntStr :: Q.Gen String
randomIntStr = show <$> randomInt

randomFloat :: Q.Gen Float
randomFloat = Q.resize 100000 (Q.arbitrary :: Q.Gen Float)

randomFloatStr :: Q.Gen String
randomFloatStr = show <$> randomFloat

randomIntConstantValue :: Q.Gen ConstantValue
randomIntConstantValue = ConstantValue <$> randomIntStr

randomFloatConstantValue :: Q.Gen ConstantValue
randomFloatConstantValue = ConstantValue <$> randomFloatStr

identifier :: Q.Gen String
identifier =
  (:) <$> randNonNumeral
    <*> Q.oneof
      [ return "",
        (:) <$> randNumeral <*> identifier,
        (:) <$> randNonNumeral <*> identifier
      ]
  where
    randNonNumeral = Q.oneof [Q.choose ('a', 'z'), Q.choose ('A', 'Z'), return '_']
    randNumeral = Q.choose ('0', '9')

data GeneratedVars = GeneratedVars
  { intVars :: [Variable],
    floatVars :: [Variable],
    intArrayVars :: [Variable],
    floatArrayVars :: [Variable],
    labels :: [Operand]
  }

genBinOpInst :: GeneratedVars -> Q.Gen Instruction
genBinOpInst (GeneratedVars intVars floatVars _ _ _) = do
  opcode' <- Q.elements [ADD, SUB, MULT, DIV, AND, OR]

  let randomIntVar = VariableOperand <$> Q.elements intVars
  let randomIntConst = flip ConstantOperand IntType <$> randomIntConstantValue
  let randomFloatVar = VariableOperand <$> Q.elements floatVars
  let randomFloatConst = flip ConstantOperand FloatType <$> randomFloatConstantValue

  intOperation <- Q.arbitrary :: Q.Gen Bool

  intDef <- randomIntVar
  intUses <- replicateM 2 (Q.oneof [randomIntVar, randomIntConst])

  floatDef <- randomFloatVar
  floatUses <- replicateM 2 (Q.oneof [randomFloatVar, randomFloatConst])

  -- floatOperands <- replicateM 2 (VariableOperand <$> Q.elements floatVars)
  lineNum <- LineNumber <$> randPositiveInt
  let operands' = if intOperation then intDef : intUses else floatDef : floatUses
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
genGotoInst (GeneratedVars _ _ _ _ labels) = do
  label' <- Q.elements labels
  lineNum <- LineNumber <$> randPositiveInt
  return (Instruction GOTO [label'] lineNum)

genPutInst :: GeneratedVars -> Q.Gen Instruction
genPutInst (GeneratedVars intVars floatVars _ _ _) = do
  fn <- Q.elements ["puti", "putc", "putf"]

  let randomIntVar = VariableOperand <$> Q.elements intVars
  let randomFloatVar = VariableOperand <$> Q.elements floatVars

  printVal <- case fn of
    "puti" -> randomIntVar
    "putc" -> randomIntVar
    "putf" -> randomFloatVar

  lineNum <- LineNumber <$> randPositiveInt

  return (Instruction CALL [FunctionOperand (FunctionName fn), printVal] lineNum)

genInst :: GeneratedVars -> Q.Gen Instruction
genInst gv = do
  let binOpInst = genBinOpInst gv
  let branchInst = genBranchInst gv
  let gotoInst = genGotoInst gv
  Q.frequency [(4, binOpInst), (1, branchInst), (1, gotoInst)]

genInst' :: GeneratedVars -> Q.Gen Instruction
genInst' gv = do
  let binOpInst = genBinOpInst gv
  let branchInst = genBranchInst gv
  let gotoInst = genGotoInst gv
  let putInst = genPutInst gv
  Q.frequency [(4, binOpInst), (1, branchInst), (1, gotoInst), (1, putInst)]

randPositiveInt :: Q.Gen Int
randPositiveInt = (Q.arbitrary :: Q.Gen Int) `Q.suchThat` (> 0)

genLabel :: Q.Gen Operand
genLabel = LabelOperand . LabelName <$> identifier

insertRandomLabels :: GeneratedVars -> [Instruction] -> Q.Gen [Instruction]
insertRandomLabels gv insts = do
  linenumbers <- sort <$> replicateM c (Q.chooseInt (1, k+c))
  let labels'' = zip linenumbers labels'
  let (xs, _, res) = foldl' f (labels'', 0, []) insts
  return $ res ++ map (\(_, operand) -> Instruction LABEL [operand] (LineNumber (-1))) xs
  where
    f :: ([(Int, Operand)], Int, [Instruction]) -> Instruction -> ([(Int, Operand)], Int, [Instruction])
    f ([], c, is) i = ([], c + 1, is ++ [i])
    f (x : xs, c, is) i = if c >= fst x then (xs, c+1, is ++ [i, Instruction LABEL [snd x] (LineNumber c)]) else (x : xs, c + 1, is ++ [i])

    labels' = labels gv
    k = length insts
    c = length labels'

data IrConfig = IrConfig
  { instrRange :: (Int, Int)
  , labelRange :: (Int, Int)
  , varRange   :: (Int, Int)
  }

-- LineNumber doesn't matter since we will be transforming instructions to IR
genUnsafeRandomFunc :: IrConfig -> Q.Gen Function
genUnsafeRandomFunc (IrConfig ir lr vr) = do
  varCount <- Q.chooseInt vr
  labelCount <- Q.chooseInt lr
  instCount <- Q.chooseInt ir

  variables <- replicateM varCount (Q.arbitrary :: Q.Gen Variable)
  labels <- replicateM varCount genLabel

  let variables' = int1 : float1 : intArr1 : floatArr1 : variables
      generatedVars = GeneratedVars intVars floatVars intArrVars floatArrVars labels
      intVars = filter (\v -> variableType v == IntType) variables'
      floatVars = filter (\v -> variableType v == FloatType) variables'
      intArrVars = filter (\v -> elemType (variableType v) == IntType) variables'
      floatArrVars = filter (\v -> elemType (variableType v) == FloatType) variables'

  ins <- replicateM instCount $ genInst generatedVars
  inst <- insertRandomLabels generatedVars ins
  return (Function (FunctionName "main") VoidType [] variables' inst)
  where
    int1 = Variable (VariableName "etuhteuh") IntType
    float1 = Variable (VariableName "tnoehtnuh") FloatType
    intArr1 = Variable (VariableName "intArr1") (ArrayType (ArraySize 1) IntType)
    floatArr1 = Variable (VariableName "floatArr1") (ArrayType (ArraySize 1) FloatType)

-- generates a function that calls put functions
-- necessary to ensure that program output does not change after optimization
-- also, puts 
genSomewhatSafeRandomFunc :: IrConfig -> Q.Gen Function
genSomewhatSafeRandomFunc (IrConfig ir lr vr) = do
  varCount <- Q.chooseInt vr
  labelCount <- Q.chooseInt lr
  instCount <- Q.chooseInt ir

  variables <- nub <$> replicateM varCount (Q.arbitrary :: Q.Gen Variable)
  labels <- nub <$> replicateM varCount genLabel

  let variables' = int1 : float1 : intArr1 : floatArr1 : variables
      generatedVars = GeneratedVars intVars floatVars intArrVars floatArrVars labels
      intVars = filter (\v -> variableType v == IntType) variables'
      floatVars = filter (\v -> variableType v == FloatType) variables'
      intArrVars = filter (\v -> elemType (variableType v) == IntType) variables'
      floatArrVars = filter (\v -> elemType (variableType v) == FloatType) variables'

  ins <- replicateM instCount (genInst' generatedVars)
  inst <- insertRandomLabels generatedVars ins
  return (Function (FunctionName "main") VoidType [] variables' inst)
  where
    int1 = Variable (VariableName "etuhteuh") IntType
    float1 = Variable (VariableName "tnoehtnuh") FloatType
    intArr1 = Variable (VariableName "intArr1") (ArrayType (ArraySize 1) IntType)
    floatArr1 = Variable (VariableName "floatArr1") (ArrayType (ArraySize 1) FloatType)

genRandomFuncHUGEEE :: Q.Gen Function
genRandomFuncHUGEEE = genSomewhatSafeRandomFunc
  IrConfig { instrRange = (600, 6000), labelRange = (50, 200), varRange = (2, 30) }

-- 1. insert labels
-- 2. constant varaibles
-- 3. assign operations

-- so we want to write this to a bunch of files
writeToFile = do
  sequence $ map (\fp -> wf ("test_huge" ++ show fp)) [1 .. 3]
  where
    wf path = do
      func <- Q.generate genRandomFuncHUGEEE
      writeFile path $ pr func

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module TigerIR.IrInstruction where

import TigerIR.Types

newtype LineNumber = LineNumber Int
  deriving (Ord, Eq)

data IrInstruction = IrInstruction
  { instruction :: Instruction
  , lineNum     :: LineNumber
  }

data Instruction
  = AssignIntVar         (AssignVarOperands IntType)
  | AssignFloatVar       (AssignVarOperands FloatType)
  | BinaryIntOperation   (BinOp IntType)
  | BinaryFloatOperation (BinOp FloatType)
  | BranchOpIntCmp       (BrOp IntType)
  | BranchOpFloatCmp     (BrOp FloatType)
  | ReturnInt            (Variable IntType)
  | ReturnFloat          (Variable FloatType)
  | Call                 FunctionName [GeneralOperand]
  | CallrInt             (Variable IntType) FunctionName [GeneralOperand]                
  | CallrFloat           (Variable FloatType) FunctionName [GeneralOperand]
  | Goto                 Label
  | IntArrStore          (ArrStoreOperands IntType IntArr) 
  | FloatArrStore        (ArrStoreOperands FloatType FloatArr)
  | IntArrLoad           (ArrLoadOperands IntType IntArr)
  | FloatArrLoad         (ArrLoadOperands FloatType FloatArr)
  | AssignIntArr         (ArrAssignOperands IntType IntArr)
  | AssignFloatArr       (ArrAssignOperands FloatType FloatArr)

data GeneralOperand
  = IntVar      (Variable IntType)
  | FloatVar    (Variable FloatType)
  | IntArrVar   (Array IntArr)
  | FloatArrVar (Array FloatArr)
  | Function    FunctionName
  | IntConst    (Constant IntType)
  | FloatConst  (Constant FloatType)

data AssignVarOperands (a :: VarType)
  = AssignVarOpsVV (Variable a) (Variable a)
  | AssignVarOpsVC (Variable a) (Constant a)

data BinOp (a :: VarType)
  = Add  (BinOperands a)
  | Sub  (BinOperands a)
  | Mult (BinOperands a)
  | Div  (BinOperands a)
  | And  (BinOperands a)
  | Or   (BinOperands a)

data BrOp (a :: VarType)
  = Breq  (BrOperands a)
  | Brneq (BrOperands a)
  | Brlt  (BrOperands a)
  | Brgt  (BrOperands a)
  | Brgeq (BrOperands a)
  | Brleq (BrOperands a)

data BinOperands (a :: VarType)
  = BinOpsVVV (Variable a) (Variable a) (Variable a)
  | BinOpsVCV (Variable a) (Constant a) (Variable a)
  | BinOpsVCC (Variable a) (Constant a) (Constant a)

data BrOperands (a :: VarType)
  = BrOpsVV Label (Variable a) (Variable a)
  | BrOpsVC Label (Variable a) (Constant a)
  | BrOpsCV Label (Constant a) (Variable a)
  | BrOpsCC Label (Constant a) (Constant a)

-- Best we can do is manually make sure that a and b are consistent
data ArrStoreOperands (a :: VarType) (b :: ArrType)
  = ArrStoreOperands (Variable a) (Array b) (Variable IntType)

data ArrLoadOperands (a :: VarType) (b :: ArrType)
  = ArrLoadOperands (Variable a) (Array b) (Variable IntType)

data ArrAssignOperands (a :: VarType) (b :: ArrType)
  = ArrAssignOperands (Array b) (Variable IntType) (Variable a)
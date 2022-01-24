module IR.Printer where
{-# LANGUAGE FlexibleContexts #-}

import IR.Type
import IR.Function
import IR.Instruction
import IR.Program

import Data.List (intersperse, intercalate)

class Print p where
  pr :: p -> String

instance Print LineNumber where
  pr (LineNumber nm) = show nm

instance Print OpCode where
  pr = show

instance Print Operand where
  pr op = case op of
    LabelOperand (LabelName s) -> s
    FunctionOperand (FunctionName s) -> s
    VariableOperand (Variable (VariableName s) vt) -> s
    ConstantOperand (ConstantValue s) tp -> s

instance Print Instruction where
  pr (Instruction opcode operands lineNum) = case opcode of
    LABEL -> (pr . head) operands ++ ":"
    otherwise -> "    " ++ pr opcode ++ concat (map ((++) ", " . pr) operands)

instance Print Type where
  pr t = case t of
    IntType -> "int"
    FloatType -> "float"
    ArrayType (ArraySize sz) t -> pr t ++ "[" ++ show sz ++ "]"
    VoidType -> "void"


instance Print Function where
  pr (Function (FunctionName fns) rt ps vs is) = concat (intersperse "\n" [
      "#start_function",
      prSig,
      prIntList,
      prFloatList,
      prInstructions,
      "#end_function"
    ])
    where
      prSig = pr rt ++ " " ++ fns ++ "(" ++ prParams ++ "):"
      prParam (Variable (VariableName s) vt) = pr vt ++ " " ++ s
      prParams = intercalate ", " $ map prParam ps
      prIntList = "int-list: " ++ intercalate ", " (map prVar (filter (isIntType . variableType) vs))
      prFloatList = "float-list: " ++ intercalate ", " (map prVar (filter (isFloatType . variableType) vs))
      prInstructions = intercalate "\n" $ map pr is
      prVar (Variable (VariableName s) vt) = case vt of
        ArrayType (ArraySize sz) _ -> s ++ "[" ++ show sz ++ "]"
        otherwise -> s

instance Print Program where
  pr (Program fns) = intercalate "\n" (map pr fns)

{-
testInst = Instruction ADD [
    VariableOperand (Variable (VariableName "a") IntType),
    VariableOperand (Variable (VariableName "b") IntType),
    --VariableOperand (Variable (VariableName "c") IntType),
    ConstantOperand (ConstantValue "123198") IntType
  ] (LineNumber 5)
-}

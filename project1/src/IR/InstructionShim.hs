--{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
{-# LANGUAGE DataKinds #-}

module IR.InstructionShim where

import qualified TigerIR.IrInstruction as T
import qualified TigerIR.Types         as T
import qualified IR.Instruction        as I
import qualified IR.Type               as I


-- TODO: Simply / write in terms of genOp
newToOldIrType :: T.IrInstruction -> I.Instruction
newToOldIrType (T.IrInstruction newIns (T.LineNumber ln)) =
  case newIns of
    -- ASSIGN Var
    T.AssignIntVar ops -> case ops of
      T.AssignVarOpsVV (T.Variable v1) (T.Variable v2) ->
          I.Instruction I.ASSIGN
            [ I.VariableOperand (I.Variable (v2v v1) I.IntType)
            , I.VariableOperand (I.Variable (v2v v2) I.IntType)
            ]
            ln'

      T.AssignVarOpsVC (T.Variable v1) (T.Constant c1) ->
        I.Instruction I.ASSIGN
          [ I.VariableOperand (I.Variable (v2v v1) I.IntType)
          , I.ConstantOperand (c2c c1) I.IntType
          ]
          ln'
    
    T.AssignFloatVar ops -> case ops of 
      T.AssignVarOpsVV (T.Variable v1) (T.Variable v2) ->
          I.Instruction I.ASSIGN
            [ I.VariableOperand (I.Variable (v2v v1) I.FloatType)
            , I.VariableOperand (I.Variable (v2v v2) I.FloatType)
            ]
            ln'
      
      T.AssignVarOpsVC (T.Variable v1) (T.Constant c1) ->
        I.Instruction I.ASSIGN
          [ I.VariableOperand (I.Variable (v2v v1) I.FloatType)
          , I.ConstantOperand (c2c c1) I.FloatType
          ]
          ln'
    
    -- Binary Operations
    T.BinaryIntOperation op ->
      case op of
        T.Add  bops -> I.Instruction I.ADD  (bi2bi bops) ln'
        T.Sub  bops -> I.Instruction I.SUB  (bi2bi bops) ln'
        T.Mult bops -> I.Instruction I.MULT (bi2bi bops) ln'
        T.Div  bops -> I.Instruction I.DIV  (bi2bi bops) ln'
        T.And  bops -> I.Instruction I.AND  (bi2bi bops) ln'
        T.Or   bops -> I.Instruction I.OR   (bi2bi bops) ln'
    
    T.BinaryFloatOperation op ->
      case op of
        T.Add  bops -> I.Instruction I.ADD  (bf2bf bops) ln'
        T.Sub  bops -> I.Instruction I.SUB  (bf2bf bops) ln'
        T.Mult bops -> I.Instruction I.MULT (bf2bf bops) ln'
        T.Div  bops -> I.Instruction I.DIV  (bf2bf bops) ln'
        T.And  bops -> I.Instruction I.AND  (bf2bf bops) ln'
        T.Or   bops -> I.Instruction I.OR   (bf2bf bops) ln'
    
    -- Branch Operations
    T.BranchOpIntCmp op ->
      case op of
        T.Breq  brops -> I.Instruction I.BREQ  (bri2bri brops) ln'
        T.Brneq brops -> I.Instruction I.BRNEQ (bri2bri brops) ln'
        T.Brlt  brops -> I.Instruction I.BRLT  (bri2bri brops) ln'
        T.Brgeq brops -> I.Instruction I.BRGEQ (bri2bri brops) ln'
        T.Brleq brops -> I.Instruction I.BRLEQ (bri2bri brops) ln'
    
    T.BranchOpFloatCmp op ->
      case op of
        T.Breq  brops -> I.Instruction I.BREQ  (brf2brf brops) ln'
        T.Brneq brops -> I.Instruction I.BRNEQ (brf2brf brops) ln'
        T.Brlt  brops -> I.Instruction I.BRLT  (brf2brf brops) ln'
        T.Brgeq brops -> I.Instruction I.BRGEQ (brf2brf brops) ln'
        T.Brleq brops -> I.Instruction I.BRLEQ (brf2brf brops) ln'

    -- Return
    T.ReturnInt (T.Variable v) ->
     I.Instruction I.RETURN
      [I.VariableOperand (I.Variable (v2v v) I.IntType)]
      ln'
    
    T.ReturnFloat (T.Variable v) ->
     I.Instruction I.RETURN
      [I.VariableOperand (I.Variable (v2v v) I.FloatType)]
      ln'

    -- Call Operations
    T.Call fn gops ->
      I.Instruction I.CALL (fn2fn fn : map genOp2genOp gops) ln'
    
    T.CallrInt (T.Variable v) fn gops ->
      let v' = I.VariableOperand (I.Variable (v2v v) I.IntType)
      in I.Instruction I.CALL (v' : fn2fn fn : map genOp2genOp gops) ln'
    
    T.CallrFloat (T.Variable v) fn gops ->
      let v' = I.VariableOperand (I.Variable (v2v v) I.FloatType)
      in I.Instruction I.CALL (v' : fn2fn fn : map genOp2genOp gops) ln'

    -- Goto
    T.Goto (T.Label s) ->
      I.Instruction I.GOTO [I.LabelOperand (I.LabelName s)] ln'

    -- Array Store
    T.IntArrStore (T.ArrStoreOperands (T.Variable v1) arr (T.Variable v2)) ->
      I.Instruction I.ARRAY_STORE
        [ I.VariableOperand (I.Variable (v2v v1) I.IntType)
        , ia2ia arr
        , I.VariableOperand (I.Variable (v2v v2) I.IntType)
        ]
        ln'

--    T.FloatArrStore (T.ArrStoreOperands (T.Variable v1) arr (T.Variable v2)) ->
--      I.Instruction I.ARRAY_STORE
--        [ I.VariableOperand (I.Variable (v2v v1) I.FloatType)
--        , ia2ia arr
--        , I.VariableOperand (I.Variable (v2v v2) I.IntType)
--        ]
--        ln'

    -- Array Load
    
  where
    ln' = I.LineNumber ln


v2v :: T.VariableName -> I.VariableName
v2v (T.VariableName v) = I.VariableName v

c2c :: T.ConstantValue -> I.ConstantValue
c2c (T.ConstantValue cv) = I.ConstantValue cv

-- r slash applying to college
ia2ia :: T.Array T.IntArr -> I.Operand
ia2ia (T.Array (T.ArrayName n) (T.ArraySize sz)) =
  I.VariableOperand
    (I.Variable
      (I.VariableName n)
      (I.ArrayType (I.ArraySize sz) I.IntType)
    )

fa2fa :: T.Array T.FloatArr -> I.Operand
fa2fa (T.Array (T.ArrayName n) (T.ArraySize sz)) =
  I.VariableOperand
    (I.Variable
      (I.VariableName n)
      (I.ArrayType (I.ArraySize sz) I.FloatType)
    )

label2label :: T.Label -> I.LabelName
label2label (T.Label s) = I.LabelName s

fn2fn :: T.FunctionName -> I.Operand
fn2fn (T.FunctionName fn) = I.FunctionOperand (I.FunctionName fn)

genOp2genOp :: T.GeneralOperand -> I.Operand
genOp2genOp op = case op of
  T.IntVar   (T.Variable v)      -> I.VariableOperand (I.Variable (v2v v) I.IntType)
  T.FloatVar (T.Variable v)      -> I.VariableOperand (I.Variable (v2v v) I.FloatType)
  T.Function (T.FunctionName fn) -> I.FunctionOperand (I.FunctionName fn)
  T.IntConst (T.Constant c)      -> I.ConstantOperand (c2c c) I.IntType
  T.FloatConst (T.Constant c)    -> I.ConstantOperand (c2c c) I.FloatType
  T.IntArrVar (T.Array (T.ArrayName n) (T.ArraySize sz)) ->
    I.VariableOperand
      (I.Variable
        (I.VariableName n)
        (I.ArrayType (I.ArraySize sz) I.IntType)
      )
  
  T.FloatArrVar (T.Array (T.ArrayName n) (T.ArraySize sz)) ->
    I.VariableOperand
      (I.Variable
        (I.VariableName n)
        (I.ArrayType (I.ArraySize sz) I.FloatType)
      )


bi2bi :: T.BinOperands T.IntType -> [I.Operand]
bi2bi ops = case ops of
  T.BinOpsVVV (T.Variable v1) (T.Variable v2) (T.Variable v3) ->
    [ I.VariableOperand (I.Variable (v2v v1) I.IntType)
    , I.VariableOperand (I.Variable (v2v v2) I.IntType)
    , I.VariableOperand (I.Variable (v2v v3) I.IntType)
    ]

  T.BinOpsVCC (T.Variable v1) (T.Constant c1) (T.Constant c2) ->
    [ I.VariableOperand (I.Variable (v2v v1) I.IntType)
    , I.ConstantOperand (c2c c1) I.IntType
    , I.ConstantOperand (c2c c2) I.IntType
    ]
  
  T.BinOpsVCV (T.Variable v1) (T.Constant c1) (T.Variable v2) ->
    [ I.VariableOperand (I.Variable (v2v v1) I.IntType)
    , I.ConstantOperand (c2c c1) I.IntType
    , I.VariableOperand (I.Variable (v2v v2) I.IntType)
    ]

bf2bf :: T.BinOperands T.FloatType -> [I.Operand]
bf2bf ops = case ops of
  T.BinOpsVVV (T.Variable v1) (T.Variable v2) (T.Variable v3) ->
    [ I.VariableOperand (I.Variable (v2v v1) I.FloatType)
    , I.VariableOperand (I.Variable (v2v v2) I.FloatType)
    , I.VariableOperand (I.Variable (v2v v3) I.FloatType)
    ]

  T.BinOpsVCC (T.Variable v1) (T.Constant c1) (T.Constant c2) ->
    [ I.VariableOperand (I.Variable (v2v v1) I.FloatType)
    , I.ConstantOperand (c2c c1) I.FloatType
    , I.ConstantOperand (c2c c2) I.FloatType
    ]
  
  T.BinOpsVCV (T.Variable v1) (T.Constant c1) (T.Variable v2) ->
    [ I.VariableOperand (I.Variable (v2v v1) I.FloatType)
    , I.ConstantOperand (c2c c1) I.FloatType
    , I.VariableOperand (I.Variable (v2v v2) I.FloatType)
    ]

bri2bri :: T.BrOperands T.IntType -> [I.Operand]
bri2bri brops = case brops of
  T.BrOpsVV label (T.Variable v1) (T.Variable v2) -> 
    [ I.LabelOperand (label2label label)
    , I.VariableOperand (I.Variable (v2v v1) I.IntType)
    , I.VariableOperand (I.Variable (v2v v2) I.IntType)
    ]
  T.BrOpsVC label (T.Variable v1) (T.Constant c1) -> 
    [ I.LabelOperand (label2label label)
    , I.VariableOperand (I.Variable (v2v v1) I.IntType)
    , I.ConstantOperand (c2c c1) I.IntType
    ]
  T.BrOpsCV label (T.Constant c1) (T.Variable v1) ->
    [ I.LabelOperand (label2label label)
    , I.ConstantOperand (c2c c1) I.IntType
    , I.VariableOperand (I.Variable (v2v v1) I.IntType)
    ]
  T.BrOpsCC label (T.Constant c1) (T.Constant c2) ->
    [ I.LabelOperand (label2label label)
    , I.ConstantOperand (c2c c1) I.IntType
    , I.ConstantOperand (c2c c2) I.IntType
    ]

brf2brf :: T.BrOperands T.FloatType -> [I.Operand]
brf2brf brops = case brops of
  T.BrOpsVV label (T.Variable v1) (T.Variable v2) -> 
    [ I.LabelOperand (label2label label)
    , I.VariableOperand (I.Variable (v2v v1) I.FloatType)
    , I.VariableOperand (I.Variable (v2v v2) I.FloatType)
    ]
  T.BrOpsVC label (T.Variable v1) (T.Constant c1) -> 
    [ I.LabelOperand (label2label label)
    , I.VariableOperand (I.Variable (v2v v1) I.FloatType)
    , I.ConstantOperand (c2c c1) I.FloatType
    ]
  T.BrOpsCV label (T.Constant c1) (T.Variable v1) ->
    [ I.LabelOperand (label2label label)
    , I.ConstantOperand (c2c c1) I.FloatType
    , I.VariableOperand (I.Variable (v2v v1) I.FloatType)
    ]
  T.BrOpsCC label (T.Constant c1) (T.Constant c2) ->
    [ I.LabelOperand (label2label label)
    , I.ConstantOperand (c2c c1) I.FloatType
    , I.ConstantOperand (c2c c2) I.FloatType
    ]
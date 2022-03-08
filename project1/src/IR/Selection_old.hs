--module IR.Selection where

import IR.Function
import IR.Program
import IR.Instruction
import qualified Data.Map as M
import Data.Bits

-- notes
-- arrays will be passed as pointers, ints as values, onto the stack
-- at the start of a function we allocate space on stack for each variable
-- variables have an associated register and pointer on the stack. 
--   this can be determined directly from ir.
--   we don't care about variable size after stack allocation, each fits in a word

newtype AsmProgram = AsmProgram { asmInstructions :: [AsmInstruction] } deriving ()

newtype Reg = Reg String deriving (Show, Eq)
newtype Imm = Imm String deriving (Show, Eq)
newtype Lab = Lab String deriving (Show, Eq)
-- newtype RetRegister = RetReg ()
zreg = Reg "$0"
imreg = Reg "$8"
ra = Reg "$31"
v0 = Reg "$2"
v1 = Reg "$3"
sp = Reg "$29"
fp = Reg "$30"
t0 = Reg "$8"
t1 = Reg "$9"
t2 = Reg "$10"
t3 = Reg "$11"
t4 = Reg "$12"
t5 = Reg "$13"
t6 = Reg "$14"
t7 = Reg "$15"
t8 = Reg "$24"
t9 = Reg "$25"


data MipsVirtual
  = Addi Reg Reg Imm
  | Add Reg Reg Reg
  | Sub Reg Reg Reg
  | Mflo Reg
  | Mult Reg Reg
  | Div Reg Reg
  | Andi Reg Reg Imm
  | And Reg Reg Reg
  | Ori Reg Reg Imm
  | Or Reg Reg Reg
  | Jal Lab
  | Jr Reg
  | Beq Reg Reg Lab
  | Bne Reg Reg Lab
  | Bgtz Reg Lab
  | Blez Reg Lab
  | Lw Reg Imm Reg
  | Sw  Reg Imm Reg
  | Label Lab
  | Placeholder Instruction
  -- deriving (Show)

addVals str1 str2 = show $ read str1 + read str2
subVals str1 str2 = show $ read str1 - read str2
mulVals str1 str2 = show $ read str1 * read str2
divVals str1 str2 = show $ (read str1 :: Integer) `quot` read str2
andVals str1 str2 = show $ (read str1 :: Integer) .&. read str2
orVals str1 str2 = show $ (read str1 :: Integer) .|. read str2

constValToImm (ConstantValue str) = Imm str
constValToNegatedImm (ConstantValue str) = Imm $ show (negate (read str) :: Integer)

toAsm :: Program -> AsmProgram
toAsm = AsmProgram . programSelection

allocateRegs :: Function -> M.Map Variable Reg
allocateRegs (Function _ _ params vars _) = foldl comb M.empty (params ++ vars)
  where
    -- TODO: smarter allocation here? r8 is used for some immediate ops in instruction selection
    comb mp var = M.insert var (Reg ("$" ++ show (length mp + 32))) mp

programSelection :: Program -> [AsmInstruction]
programSelection prog = concatMap functionSelection (functions prog)

functionSelection :: Function -> [AsmInstruction]
functionSelection fn@(Function (FunctionName fname) _ _ _ _) = Label (Lab fname) : concatMap sel (instrs fn)
  where
    reg = (M.!) (allocateRegs fn)

    sel :: Instruction -> [AsmInstruction]
    sel (Instruction ASSIGN [VariableOperand t, ConstantOperand (ConstantValue str) _] _) = [Addi (reg t) zreg (Imm str)]
    sel (Instruction ASSIGN [VariableOperand t, VariableOperand s] _) = [Addi (reg t) (reg s) (Imm "0")]

    sel (Instruction ADD [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ addVals str1 str2)]
    sel (Instruction ADD [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) (reg s) (Imm str2)]
    sel (Instruction ADD [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi (reg t) (reg s) (Imm str1)]
    sel (Instruction ADD [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Add (reg d) (reg s) (reg t)]

    -- no immediate op, so we use imreg as temp
    sel (Instruction SUB [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ subVals str1 str2)]
    sel (Instruction SUB [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Sub (reg t) (reg s) imreg]
    sel (Instruction SUB [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi (reg t) zreg (Imm str1), Sub (reg t) (reg t) (reg s)]
    sel (Instruction SUB [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Sub (reg d) (reg s) (reg t)]

    sel (Instruction MULT [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ mulVals str1 str2)]
    sel (Instruction MULT [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Mult (reg s) imreg, Mflo (reg t)]
    sel (Instruction MULT [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi imreg zreg (Imm str1), Mult imreg (reg s), Mflo (reg t)]
    sel (Instruction MULT [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Mult (reg s) (reg t), Mflo (reg d)]

    -- div similar to mult
    sel (Instruction DIV [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ divVals str1 str2)]
    sel (Instruction DIV [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Div (reg s) imreg, Mflo (reg t)]
    sel (Instruction DIV [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Addi imreg zreg (Imm str1), Div imreg (reg s), Mflo (reg t)]
    sel (Instruction DIV [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Div (reg s) (reg t), Mflo (reg d)]

    -- and similar to add
    sel (Instruction AND [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ andVals str1 str2)]
    sel (Instruction AND [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Andi (reg t) (reg s) (Imm str2)]
    sel (Instruction AND [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Andi (reg t) (reg s) (Imm str1)]
    sel (Instruction AND [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [And (reg d) (reg s) (reg t)]

    -- or similar to and
    sel (Instruction OR [VariableOperand t, ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _) =
      [Addi (reg t) zreg (Imm $ orVals str1 str2)]
    sel (Instruction OR [VariableOperand t, VariableOperand s, ConstantOperand (ConstantValue str2) _] _) =
      [Ori (reg t) (reg s) (Imm str2)]
    sel (Instruction OR [VariableOperand t, ConstantOperand (ConstantValue str1) _, VariableOperand s] _) =
      [Ori (reg t) (reg s) (Imm str1)]
    sel (Instruction OR [VariableOperand d, VariableOperand s, VariableOperand t] _) =
      [Or (reg d) (reg s) (reg t)]

    sel (Instruction BREQ [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _)
      | (read str1::Integer) == read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BREQ [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, VariableOperand z] _) =
      [Addi imreg zreg (Imm str1), Beq imreg (reg z) (Lab str)]
    sel (Instruction BREQ [LabelOperand (LabelName str), VariableOperand y, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Beq (reg y) imreg (Lab str)]
    sel (Instruction BREQ [LabelOperand (LabelName str), VariableOperand y, VariableOperand z] _) =
      [Beq (reg y) (reg z) (Lab str)]

    sel (Instruction BRNEQ [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _)
      | (read str1::Integer) /= read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRNEQ [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, VariableOperand z] _) =
      [Addi imreg zreg (Imm str1), Bne imreg (reg z) (Lab str)]
    sel (Instruction BRNEQ [LabelOperand (LabelName str), VariableOperand y, ConstantOperand (ConstantValue str2) _] _) =
      [Addi imreg zreg (Imm str2), Bne (reg y) imreg (Lab str)]
    sel (Instruction BRNEQ [LabelOperand (LabelName str), VariableOperand y, VariableOperand z] _) =
      [Bne (reg y) (reg z) (Lab str)]

    sel (Instruction BRLT [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _)
      | (read str1::Integer) < read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRLT [LabelOperand (LabelName str), ConstantOperand cv1 _, VariableOperand z] _) =
      [Addi imreg (reg z) (constValToNegatedImm cv1), Bgtz imreg (Lab str)]
    sel (Instruction BRLT [LabelOperand (LabelName str), VariableOperand y, ConstantOperand cv2 _] _) =
      [Addi imreg (reg y) (constValToNegatedImm cv2), Sub imreg zreg imreg, Bgtz imreg (Lab str)]
    sel (Instruction BRLT [LabelOperand (LabelName str), VariableOperand y, VariableOperand z] _) =
      [Sub imreg (reg y) (reg z), Bgtz imreg (Lab str)]

    sel (Instruction BRGT [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _)
      | (read str1::Integer) > read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRGT [LabelOperand (LabelName str), ConstantOperand cv1 _, VariableOperand z] _) =
      [Addi imreg (reg z) (constValToNegatedImm cv1), Sub imreg zreg imreg, Bgtz imreg (Lab str)]
    sel (Instruction BRGT [LabelOperand (LabelName str), VariableOperand y, ConstantOperand cv2 _] _) =
      [Addi imreg (reg y) (constValToNegatedImm cv2), Bgtz imreg (Lab str)]
    sel (Instruction BRGT [LabelOperand (LabelName str), VariableOperand y, VariableOperand z] _) =
      [Sub imreg (reg z) (reg y), Bgtz imreg (Lab str)]

    sel (Instruction BRGEQ [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _)
      | (read str1::Integer) >= read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRGEQ [LabelOperand (LabelName str), ConstantOperand cv1 _, VariableOperand z] _) =
      [Addi imreg (reg z) (constValToNegatedImm cv1), Blez imreg (Lab str)]
    sel (Instruction BRGEQ [LabelOperand (LabelName str), VariableOperand y, ConstantOperand cv2 _] _) =
      [Addi imreg (reg y) (constValToNegatedImm cv2), Sub imreg zreg imreg, Blez imreg (Lab str)]
    sel (Instruction BRGEQ [LabelOperand (LabelName str), VariableOperand y, VariableOperand z] _) =
      [Sub imreg (reg z) (reg y), Blez imreg (Lab str)]

    sel (Instruction BRLEQ [LabelOperand (LabelName str), ConstantOperand (ConstantValue str1) _, ConstantOperand (ConstantValue str2) _] _)
      | (read str1::Integer) <= read str2 = [Jal (Lab str)]
      | otherwise = []
    sel (Instruction BRLEQ [LabelOperand (LabelName str), ConstantOperand cv1 _, VariableOperand z] _) =
      [Addi imreg (reg z) (constValToNegatedImm cv1), Sub imreg zreg imreg, Blez imreg (Lab str)]
    sel (Instruction BRLEQ [LabelOperand (LabelName str), VariableOperand y, ConstantOperand cv2 _] _) =
      [Addi imreg (reg y) (constValToNegatedImm cv2), Blez imreg (Lab str)]
    sel (Instruction BRLEQ [LabelOperand (LabelName str), VariableOperand y, VariableOperand z] _) =
      [Sub imreg (reg y) (reg z), Blez imreg (Lab str)]

    -- array load
    sel (Instruction ARRAY_LOAD [VariableOperand t, VariableOperand a, ConstantOperand (ConstantValue o) _] _) =
      [Lw (reg t) (Imm $ mulVals o "4") (reg a)]
    sel (Instruction ARRAY_LOAD [VariableOperand t, VariableOperand a, VariableOperand o] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm "4"),
        Mult imreg (reg o),
        Mflo (reg o),
        Add (reg o) (reg o) (reg a),
        Lw (reg t) (Imm "0") (reg o)]

    -- array store
    sel (Instruction ARRAY_STORE [VariableOperand s, VariableOperand a, ConstantOperand (ConstantValue o) _] _) =
      [Sw (reg s) (Imm $ mulVals o "4") (reg a)]
    sel (Instruction ARRAY_STORE [VariableOperand s, VariableOperand a, VariableOperand o] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm "4"),
        Mult imreg (reg o),
        Mflo (reg o),
        Add (reg o) (reg a) (reg o),
        Sw (reg s) (Imm "0") (reg o)]
    sel (Instruction ARRAY_STORE [ConstantOperand (ConstantValue s) _, VariableOperand a, VariableOperand o] _) =
      [ And imreg zreg imreg,
        Addi imreg imreg (Imm "4"),
        Mult (reg o) imreg,
        Mflo imreg,
        Add (reg a) (reg a) imreg,
        And imreg zreg imreg,
        Addi imreg imreg (Imm s),
        Sw imreg (Imm "o") (reg a)]
    sel (Instruction ARRAY_STORE [ConstantOperand (ConstantValue s) _, VariableOperand a, ConstantOperand (ConstantValue o) _] _) =
      [ And imreg zreg imreg
      , Addi imreg imreg (Imm s)
      , Sw imreg (Imm (mulVals o "4")) (reg a)]

    -- label 
    sel (Instruction LABEL [LabelOperand (LabelName label)] _) = [ Label (Lab ( labelStr label )) ]

    sel (Instruction GOTO [LabelOperand (LabelName str)] _) = [Jal (Lab str)]

    -- return
    sel (Instruction RETURN [VariableOperand a] _)
      = [ And v0 v0 zreg
        , Add v0 v0 (reg a)
        , Jr ra ]
    sel (Instruction RETURN [ConstantOperand (ConstantValue s) _] _)
      = [ And v0 v0 zreg
        , Addi v0 v0 (Imm s)
        , Jr ra ]

    -- call
    sel (Instruction CALL ((FunctionOperand (FunctionName fname)):args) _)
      = stackSetup ++ pushArgsToStack args ++ [ Jal (Lab fname) ]

    -- callr 
    sel (Instruction CALLR ((VariableOperand dst):(FunctionOperand (FunctionName fname)):args) _)
      = stackSetup
        ++ pushArgsToStack args
        ++ [ Jal(Lab fname)
           , Addi (reg dst) v0 (Imm "0") ] -- we should also include teardown afterwards

    sel _ = error "Failed..."
    -- sel inst = [Placeholder inst]

    labelStr :: String -> String
    labelStr labelName = show (name fn) ++ "_" ++ labelName

    pushArgsToStack :: [Operand] -> [AsmInstruction]
    pushArgsToStack operands = foldl operandsToRegs [] operands
      where
        operandsToRegs :: [AsmInstruction] -> Operand -> [AsmInstruction]
        operandsToRegs insts (VariableOperand a)
          = insts ++ [Sw (reg a) (Imm "0") sp] ++ pushToStack [reg a]
        operandsToRegs insts (ConstantOperand (ConstantValue s) _)
          = insts ++ [Addi t0 zreg (Imm s), Sw t0 (Imm "0") sp] ++ pushToStack [t0]
        operandsToRegs insts _
          = error "BIG BRUH MOMENT, higher order functions??!!?!!??"

    pushToStack :: [Reg] -> [AsmInstruction]
    pushToStack regs = foldl store [] regs
      where
        store :: [AsmInstruction] -> Reg -> [AsmInstruction]
        store insts reg = insts ++ [Sw reg (Imm (show $ 4 * (length insts + 1) )) sp]

    stackSetup :: [AsmInstruction]
    stackSetup = [ Sw t0 (Imm "4") sp
                 , Sw t1 (Imm "8") sp
                 , Sw t2 (Imm "12") sp
                 , Sw t3 (Imm "16") sp
                 , Sw t4 (Imm "20") sp
                 , Sw t5 (Imm "24") sp
                 , Sw t6 (Imm "28") sp
                 , Sw t7 (Imm "32") sp
                 , Sw t8 (Imm "36") sp
                 , Sw t9 (Imm "40") sp
                 , Sw ra (Imm "44") sp
                 , Sw fp (Imm "48") sp
                 , Addi sp sp (Imm "48") ]


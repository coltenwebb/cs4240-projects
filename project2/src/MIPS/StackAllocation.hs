module MIPS.StackAllocation where

import qualified Data.Map as M

-- import MIPS.Virtual
import TigerIR.Program

type Offset = Int

-- vmap :: TigerIrFunction -> M.Map Variable Offset
-- vmap fn = M.union (vmapParams fn) (vmapLocalVars fn)
--   where
--     vmapParams (Parameters params) = head (foldl' fparams (M.empty, 0) params)
--     vmapLocalVars (LocalVars localVars) = head (foldl' flocalvars (M.empty, 0) localVars)
--     fparams :: InitVar -> (M.Map Variable Offset, Offset) -> (M.Map Variable Offset, Offset)
--     fparams (InitV var) (mp, offset) = (M.insert var offset mp, offset + 4)
--     fparams (InitA (Array var (ArraySize size))) (mp, offset) = (M.insert var offset mp, offset + size * 4)
--     flocalvars :: InitVar -> (M.Map Variable Offset, Offset) -> (M.Map Variable Offset, Offset)
--     flocalvars (InitV var) (mp, offset) = (M.insert var offset mp, offset - 4)
--     flocalvars (InitA (Array var (ArraySize size))) (mp, offset) = (M.insert var offset mp, offset - size * 4)

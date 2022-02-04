{-# LANGUAGE DerivingVia #-}

module Fuzz.Test where

import qualified Test.QuickCheck as Q
import IR.Instruction
import IR.Type
import Data.List as L (foldl', reverse, head)
import Data.List.NonEmpty as NE hiding (map)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Data.Maybe (mapMaybe)
import IR.Optimizer.CFG
-- import IR.Optimizer.CFG (CfgAdjMap)s

data Tree = Leaf Int | Node Tree Int Tree deriving (Eq, Show)

instance Q.Arbitrary Tree where
  arbitrary = Q.sized go
    where go 0 = Leaf <$> Q.arbitrary
          go n = Q.oneof [Leaf <$> Q.arbitrary, Node <$> go' <*> Q.arbitrary <*> go']
            where go' = go (n-1)

-- instance Q.Arbitrary BlockId where 
--   arbitrary = Q.sized $ \n -> BlockId <$> Q.choose (0, n) 

-- genArr :: (Q.Arbitrary a) => Q.Gen [a]
-- genArr = Q.sized $ \n ->
--   Q.frequency
--     [ (1, return [])
--     , (3, (:) <$> genBlockId n <*> genArr)
--     ]

-- -- mySet :: Q.Gen (S.Set BlockId)
-- -- mySet = fmap S.fromList myIntList

-- genBlockId :: Int -> Q.Gen BlockId
-- genBlockId n = BlockId <$> Q.choose (0, n)

-- M.fromList (zip (map BlockId [0..n]) (  ))

-- 1. generate random # of function arg, int var, and float var
-- 2. generate random insts based on such var
-- 3. find branch, goto instructions, which will be branching or going to random labels
-- 4. insert random labels in the program

-- instance Q.Arbitrary Type where
--   arbitrary = arbArrayType >>= \arrType -> Q.elements [IntType, FloatType, VoidType, arrType]
--     where 
--       arbArrayType = ArrayType <$> arbArraySize <*> (Q.arbitrary :: Q.Gen Type)
--       arbArraySize = ArraySize <$> (Q.arbitrary :: Q.Gen Integer)

instance Q.Arbitrary Variable where
  arbitrary = Variable <$> arbVariableName <*> arbVariableType
    where
      arbVariableType = arbArrayType >>= \arrType -> Q.elements [IntType, FloatType, arrType]
      arbArrayType = ArrayType <$> arbArraySize <*> Q.elements [IntType, FloatType]
      arbArraySize = ArraySize <$> (Q.arbitrary :: Q.Gen Integer)

instance Q.Arbitrary VariableName where 
  arbitrary :: Gen [Char]
  arbitrary = Q.oneof 
    [ return []
    , (:) <$> Q.choose ('0','9') <*> (arbitrary :: VariableName)
    , (:) <$> Q.choose ('a','z')
    , (:) <$> Q.choose ('A','Z')
    , return '-'
    ]



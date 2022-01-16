module IR.Program where

import IR.Function

newtype Program = Program [Function] deriving Show
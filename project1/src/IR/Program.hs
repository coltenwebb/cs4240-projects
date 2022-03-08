module IR.Program where

import IR.Function

newtype Program = Program { functions :: [Function] } deriving Show
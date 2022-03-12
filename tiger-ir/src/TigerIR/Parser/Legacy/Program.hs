module TigerIR.Parser.Legacy.Program where

import TigerIR.Parser.Legacy.Function

newtype Program = Program { functions :: [Function] } deriving Show
module Main where

import System.Environment
import System.Exit
import Control.Monad

data Optimizer = Naive | Intra
type InputFile = FilePath
type OutputFile = FilePath
data ProgArgs = ProgArgs Optimizer InputFile OutputFile

main :: IO ()
main = do
    mainProgName <- getProgName
    maybeArgs <- arg''parse'' <$> getArgs
    return ()

    --let args = case maybeArgs of
    --  Nothing -> error $ "Expected usage: "
    --   ++ mainProgName
    --   ++ " [naive|intra] $input_ir $output_asm"
    --  Just arg -> arg

  where
    -- validating instead of "parsing" :(
    arg''parse'' :: [String] -> Maybe ProgArgs
    arg''parse'' args = case args of
      ["naive", input, output] -> Just $ ProgArgs Naive input output
      ["intra", input, output] -> Just $ ProgArgs Intra input output
      _                        -> Nothing




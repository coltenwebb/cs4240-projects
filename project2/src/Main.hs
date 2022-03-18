module Main where

import System.Environment
import System.IO
import System.Exit
import Control.Monad

import TigerIR.Parser.Reader
import MIPS.Backend
import MIPS.Printer (pr)

data Allocator = Naive | Intra
type InputFile = FilePath
type OutputFile = FilePath
data ProgArgs = ProgArgs Allocator InputFile OutputFile

main :: IO ()
main = do
    mainProgName <- getProgName
    maybeArgs <- arg''parse'' <$> getArgs

    case maybeArgs of
      Nothing -> error $ "Expected usage: "
       ++ mainProgName
       ++ " [naive|intra] $input_ir $output_asm"
      Just args -> runBackend args
  where
    -- validating instead of "parsing" :(
    arg''parse'' :: [String] -> Maybe ProgArgs
    arg''parse'' args = case args of
      ["naive", input, output] -> Just $ ProgArgs Naive input output
      ["intra", input, output] -> Just $ ProgArgs Intra input output
      _                        -> Nothing

runBackend :: ProgArgs -> IO ()
runBackend (ProgArgs alc inpt outpt) = do
  tigerIrProgE <- readProgramFile inpt

  let tigerIrProg = case tigerIrProgE of
                      Left err -> error $ show err
                      Right prog -> prog

      asmProg = case alc of
                  Naive -> naiveProgramSelection tigerIrProg
                  Intra -> error "TODO"

      asmProgStr = pr asmProg
  
  writeFile outpt asmProgStr

module Main where

import System.Environment
import System.Exit
import Control.Monad

import IR.Reader (readProgramFile)
import IR.Function as F
import IR.Program
import IR.Optimizer.CFG
import IR.Optimizer.ReachingDefs (ReachDefSets(ReachDefSets), runReachingDefAlgorithm)
import IR.Optimizer.MarkSweep

main :: IO ()
main = do
  -- not to be confused with the program we will be parsing / optimizing
  mainProgName <- getProgName
  args <- getArgs

  when (null args) $ do
    error $ "Expected usage: " ++ mainProgName ++ " [program file]"

  let progPath = head args

  eitherProg <- readProgramFile progPath
  prog <- case eitherProg of
    Left e -> error $
      "Parse failed, could be invalid path or program contents, idk "
      ++ show e
    Right prog -> return prog

  -- only try to optimize first function for now
  when (null (functions prog)) $ do
    putStrLn "Empty program, nothing to do..."
    exitSuccess

  let f1 = head (functions prog)
      ins1 = F.instrs f1
      f1' = markSweepWithReachDef f1

  putStrLn "After mark sweep with reach def (!!11!1111): "
  print f1'

  --putStrLn "Resulting sets: "
  --print rdRes

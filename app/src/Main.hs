module Main where

import System.Environment
import System.Exit
import Control.Monad

import IR.Reader (readProgramFile)
import IR.Function as F
import IR.Program
import IR.Optimizer.CFG
import IR.Optimizer.ReachingDefs
import IR.Optimizer.MarkSweep
import IR.Printer

import qualified Data.Map as M

main :: IO ()
main = do
  -- not to be confused with the program we will be parsing / optimizing
  mainProgName <- getProgName
  args <- getArgs

  when (null args) $ do
    error $ "Expected usage: " ++ mainProgName ++ " [program file]"
  
  --case head args of
    --"-t" -> testMain $ args!!1
    --"-r" -> rawMain $ args!!1
  rawMain $ head args



--testMain :: String => IO()
testMain progPath = do
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
      f1'' = simpleMarkSweep f1
      cfg' = makeCFG (F.instrs f1)
      rdRes = runReachingDefAlgorithm cfg'

  putStrLn "After mark sweep with reach def (!!11!1111): "
  print f1'
  putStrLn "Statistics:"
  putStrLn $ "og ins cnt: " ++ (show . length . F.instrs) f1
  putStrLn $ "opt ins cnt (reach def): " ++ (show . length . F.instrs) f1'
  putStrLn $ "og ins cnt (simple): " ++ (show . length . F.instrs) f1''
--
----      putStrLn $ "Original instruction count: " ++ (show . length . F.instrs)  f1
----      putStrLn $ "Optimized instruction count reach def: " ++ (show . length . F.instrs) f1'
----      putStrLn $ "Optimized instruction count simple: " ++ (show . length . F.instrs) f1''
----
  putStrLn "Resulting sets: "
  print $ M.lookup (BlockId 1) (inSets rdRes)
--


--rawMain :: String => IO ()
rawMain progPath = do
  eitherProg <- readProgramFile progPath
  prog <- case eitherProg of
    Left e -> error $ "Parse failed:\n" ++ show e
    Right prog -> return prog

  putStrLn $ pr $ optimizeProgram prog
    where
      optimizeProgram (Program fns) = Program $ map markSweepWithReachDef fns

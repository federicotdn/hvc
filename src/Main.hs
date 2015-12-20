module Main where

import System.Environment

import Args
import Init
import Commit
import Log
import Checkout
import Status

main :: IO ()
main = do
  args <- getArgs
  action <- parseArgs args
  runAction action

runAction :: HvcArgsResult -> IO()
runAction (HvcError e) = printError e
runAction (HvcOperation op) = runOperation op

runOperation :: HvcOperationType -> IO ()
runOperation (Init dir) = initHvc dir
runOperation (Commit dir msg) = commitHvc dir msg
runOperation (Log dir) = logHvc dir
runOperation (Checkout dir commit) = checkoutHvc dir commit
runOperation (Status dir) = statusHvc dir
runOperation (Help) = printHelp

printError :: HvcErrorType -> IO ()
printError (DirError dir) = putStrLn $ "Invalid directory: " ++ dir

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: hvc <directory> <operation> [options]"
  putStrLn "Valid operations are: init, commit, checkout, log, status, help"
  putStrLn "See README.md for more details."
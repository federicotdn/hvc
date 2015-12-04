module Main where

import System.Environment

import Args
import Init
import Commit
import Checkout
import Log

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
runOperation (Help) = printHelp
runOperation (Log dir) = logHvc dir
runOperation (Checkout dir commit) = checkoutHvc dir commit

printError :: HvcErrorType -> IO ()
printError (DirError dir) = putStrLn $ "Invalid directory: " ++ dir

printHelp :: IO ()
printHelp = putStrLn "help"
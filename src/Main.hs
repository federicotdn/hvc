module Main where

import System.Environment
import System.FilePath ((</>))

import Args
import Init
import Commit
import Checkout
import Hash
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
runOperation (Hash dir file) = (fileSHA1 $ dir </> file) >>= (putStrLn . bstrToHex)

printError :: HvcErrorType -> IO ()
printError (DirError dir) = putStrLn $ "Invalid directory: " ++ dir

printHelp :: IO ()
printHelp = putStrLn "help"
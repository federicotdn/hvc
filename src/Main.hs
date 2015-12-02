module Main where

import System.Environment
import System.Exit
import System.IO
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)
import Args

main :: IO ()
main = do
  args <- getArgs
  action <- parseArgs args
  runAction action

bstrToHex :: Strict.ByteString -> String
bstrToHex bytes = concat $ map (printf "%02x") (Strict.unpack bytes)

runAction :: HvcArgsResult -> IO()
runAction (HvcError e) = printError e
runAction (HvcOperation op) = runOperation op

runOperation :: HvcOperationType -> IO ()
runOperation (Init dir) = putStrLn $ "init dir" ++ dir
runOperation (Commit dir) = putStrLn $ "commit dir" ++ dir
runOperation (Help) = putStrLn "help"
runOperation (Hash dir file) = do
  contents <- Lazy.readFile file
  putStrLn $ bstrToHex (SHA1.hashlazy contents)

printError :: HvcErrorType -> IO ()
printError (DirError dir) = putStrLn $ "Invalid directory: " ++ dir
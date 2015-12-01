module Main where

import System.Environment
import System.Exit
import System.IO
import qualified Crypto.Hash.SHA1 as SHA1

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["init", dir] = putStrLn "dir"
parse ["commit", dir] = putStrLn "commit"
parse ["hash", dir] = usage
parse xs = usage


usage :: IO ()
usage = putStrLn "usage"
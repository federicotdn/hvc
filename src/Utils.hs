module Utils
       (hvcDir
       ,commitsDir
       ,objectsDir
       ,headPath
       ,printHvcDirError
       ,storeCommitHead
       ,readCommitHead
       ,execIfHvc
       ,loadCommit
       ,CommitSummary(..)) where

import System.FilePath (combine, (</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import Control.Monad (forM)
import System.IO (withFile, hPutStrLn, hGetLine,IOMode(..))

import DirTree

data CommitSummary = CommitSummary String String String deriving (Show, Read)

hvcDir :: FilePath -> FilePath
hvcDir dir = combine dir ".hvc"

commitsDir :: FilePath -> FilePath
commitsDir dir = hvcDir dir </> "commits"

objectsDir :: FilePath -> FilePath
objectsDir dir = hvcDir dir </> "objects"

headPath :: FilePath -> FilePath
headPath dir = hvcDir dir </> "HEAD"

storeCommitHead :: FilePath -> String -> IO ()
storeCommitHead base hash = do
  withFile (headPath base) WriteMode (\file -> hPutStrLn file hash)

readCommitHead :: FilePath -> IO String
readCommitHead base = do
  withFile (headPath base) ReadMode (\file -> hGetLine file)

loadCommit :: FilePath -> IO (DirTree String)
loadCommit path = do
  contents <- readFile path
  let line = (lines contents)!!1
  return $ read line

printHvcDirError :: IO ()
printHvcDirError = putStrLn "Unable to perform operation: hvc directory (.hvc) not found."

hasHvcDir :: FilePath -> IO Bool
hasHvcDir dir = do
  let dirList = [hvcDir dir, hvcDir dir </> "commits", hvcDir dir </> "objects"]
  dirsExist <- forM dirList doesDirectoryExist
  headExists <- doesFileExist $ hvcDir dir </> "HEAD"
  return (and $ headExists : dirsExist)

execIfHvc :: FilePath -> IO () -> IO ()
execIfHvc dir comp = do
  hvcEnabled <- hasHvcDir dir
  if hvcEnabled
    then comp
    else printHvcDirError
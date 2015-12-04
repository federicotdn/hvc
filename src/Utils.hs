module Utils
       (hvcDir
       ,commitsDir
       ,objectsDir
       ,headPath
       ,printHvcDirError
       ,storeCommitHead
       ,execIfHvc
       ,CommitLine(..)
       ,CommitSummary(..)) where

import System.FilePath (combine, (</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import Control.Monad (forM)
import System.IO (withFile, hPutStrLn, IOMode(..))
import Data.Time

data CommitLine = CommitLine String String deriving (Show, Read)
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
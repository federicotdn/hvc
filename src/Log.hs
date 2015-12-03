module Log (logHvc) where

import System.IO (withFile, hGetLine, IOMode(..))
import System.Directory (getDirectoryContents)
import Control.Monad (forM)
import System.FilePath (combine, (</>))

import Utils

loadCommitSummary :: FilePath -> IO CommitSummary
loadCommitSummary path = withFile path ReadMode $ \f -> do
  line <- hGetLine f
  return $ read line

getCommitPaths :: FilePath -> IO [FilePath]
getCommitPaths dir = do
  let commitDir = commitsDir dir
  commits <- getDirectoryContents commitDir
  return $ map (combine commitDir) $ filter (`notElem` [".", ".."]) commits 

execLog :: FilePath -> IO ()
execLog dir = do
  summaries <- forM (getCommitPaths dir) $ \path -> do
    (CommitSummary msg date hash) <- loadCommitSummary path
    putStrLn ""
    return "asdf"
  return ()

logHvc :: FilePath -> IO ()
logHvc dir = execIfHvc dir (execLog dir)
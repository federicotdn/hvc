module Log (logHvc) where

import System.IO (withFile, hGetLine, IOMode(..))
import System.Directory (getDirectoryContents)
import Control.Monad (forM)
import System.FilePath (combine)
import Data.List
import Data.Time.Clock (UTCTime)

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

loadSortedCommits :: [FilePath] -> IO [CommitSummary]
loadSortedCommits paths = do
  summaries <- forM paths loadCommitSummary
  return $ reverse $ sortOn (\(CommitSummary _ date _) -> read date :: UTCTime) summaries

execLog :: FilePath -> IO ()
execLog dir = do
  paths <- getCommitPaths dir
  sortedCommits <- loadSortedCommits paths
  summaries <- forM sortedCommits $ \(CommitSummary msg date hash) -> do
    putStrLn $ "commit " ++ hash
    putStrLn $ ">>= date: " ++ date
    putStrLn $ ">>= message: " ++ msg
    putStrLn ""
  if length summaries == 0
    then putStrLn "Log: no commits to show."
    else return ()

logHvc :: FilePath -> IO ()
logHvc dir = execIfHvc dir (execLog dir)
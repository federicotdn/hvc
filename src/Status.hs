module Status (statusHvc) where

import System.FilePath ((</>))
import qualified Data.Map.Strict as Map

import Utils
import DirTree
import DirTreeUtils

data FileStatus = Created | Deleted | Modified

compareTrees :: DirTree String -> DirTree String -> IO ()
compareTrees current stored = do
  let currentMap = Map.fromList (pathExtrasFromTree current)
  let storedMap = Map.fromList (pathExtrasFromTree stored)


execStatus :: FilePath -> IO ()
execStatus dir = do
  currentTree <- treeFromDir dir
  commitHead <- readCommitHead dir
  if length commitHead == 0 
    then compareTrees currentTree emptyTreeDir
    else do
      let commitPath = commitsDir dir </> commitHead
      compareTrees currentTree (loadCommit commitPath)

statusHvc :: FilePath -> IO ()
statusHvc dir = execIfHvc dir (execStatus dir)
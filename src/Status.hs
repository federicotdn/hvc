module Status (statusHvc) where

import System.FilePath ((</>))
import qualified Data.Map.Strict as Map

import Utils
import DirTree
import DirTreeUtils

mapDifferencesInfo :: String -> Map.Map FilePath String -> Map.Map FilePath String -> [String]
mapDifferencesInfo msg base other = map (\path -> msg ++ path) paths
  where paths = map fst (Map.toList (Map.difference base other))

listNewFiles :: Map.Map FilePath String -> Map.Map FilePath String -> [String]
listNewFiles = mapDifferencesInfo "new file created: "

listDeletedFiles :: Map.Map FilePath String -> Map.Map FilePath String -> [String]
listDeletedFiles = flip $ mapDifferencesInfo "file deleted: "

listChangedFiles :: Map.Map FilePath String -> Map.Map FilePath String -> [String]
listChangedFiles base other = 
  foldr (\(path, same) xs -> if same then xs else ("file changed: " ++ path) : xs) [] changedList
  where changedList = Map.toList $ Map.intersectionWith (==) base other

compareTrees :: DirTree String -> DirTree String -> [String]
compareTrees current stored = concat (map (\f -> f currentMap storedMap) cmpFunctions)
  where currentMap = Map.fromList (pathExtrasFromTree current)
        storedMap = Map.fromList (pathExtrasFromTree stored)
        cmpFunctions = [listNewFiles, listDeletedFiles, listChangedFiles]

execStatus :: FilePath -> IO ()
execStatus dir = do
  currentTree <- treeFromDir dir
  commitHead <- readCommitHead dir
  let currentTreeHashes = removeByteStrings currentTree
  if length commitHead == 0
    then do
      putStrLn "Status: HEAD is blank (no commits have been made)."
      putStr $ unlines (compareTrees currentTreeHashes emptyTreeDir)
    else do
      putStrLn $ "Status: commit HEAD is: " ++ commitHead
      let commitPath = commitsDir dir </> commitHead
      commitedTree <- loadCommit commitPath
      putStr $ unlines (compareTrees currentTreeHashes commitedTree)

statusHvc :: FilePath -> IO ()
statusHvc dir = execIfHvc dir (execStatus dir)
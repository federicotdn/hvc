module DirTree
       (DirTree(DirNode, FileNode)
       ,foldDirTree
       ,treeFromDir
       ,pathsFromTree
       ,filesFromTree) where

import Data.List (sort)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), normalise, makeRelative)
import Control.Monad (forM)

data DirTree = DirNode FilePath [DirTree] | FileNode FilePath deriving (Show)

foldDirTree :: (FilePath -> c -> b) -> ([b] -> c) -> (FilePath -> b) -> DirTree -> b
foldDirTree _g _k f (FileNode path) = f path
foldDirTree g k f (DirNode path dts) = g path (k (map (foldDirTree g k f) dts))

pathsFromTree :: DirTree -> [FilePath]
pathsFromTree = foldDirTree (:) concat (:[])

makeTreeRelative :: FilePath -> DirTree -> DirTree
makeTreeRelative path =
  foldDirTree (DirNode . (makeRelative path))
              id
              (FileNode . (makeRelative path))

filesFromTree :: DirTree -> [FilePath]
filesFromTree = foldDirTree (\_ y -> y) concat (:[])

-- TODO: ignore files based on extension
ignoredPath :: FilePath -> Bool
ignoredPath path = elem path [".", "..", ".hvc", ".git", ".stack-work"]

treeFromDir :: FilePath -> IO DirTree
treeFromDir dir = do
  tree <- treeFromDirAbs dir
  return $ makeTreeRelative dir tree

treeFromDirAbs :: FilePath -> IO DirTree
treeFromDirAbs dir = do
  let normDir = normalise dir
  contents <- getDirectoryContents normDir
  let filteredContents = filter (not . ignoredPath) contents
  nodes <- forM (sort filteredContents) $ \name -> do
    let path = normDir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then treeFromDirAbs path
      else return (FileNode path)
  return (DirNode normDir nodes)
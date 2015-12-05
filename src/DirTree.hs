module DirTree
       (DirTree(DirNode, FileNode)
       ,foldDirTree
       ,mapDirTree
       ,treeFromDir
       ,dirTreeExtraList
       ,isEmpty) where

import Data.List (sort)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), normalise, makeRelative)
import Control.Monad (forM)

data DirTree a = DirNode FilePath [DirTree a] | FileNode FilePath a deriving (Show)

foldDirTree :: (FilePath -> c -> b) -> ([b] -> c) -> (FilePath -> a -> b) -> DirTree a -> b
foldDirTree _g _k f (FileNode path extra) = f path extra
foldDirTree g k f (DirNode path dts) = g path (k (map (foldDirTree g k f) dts))

mapDirTree :: (FilePath -> FilePath) -> (FilePath -> a -> (FilePath, b)) -> DirTree a -> DirTree b
mapDirTree f g = foldDirTree (DirNode . f) id (\x y -> uncurry FileNode $ g x y)

dirTreeExtraList :: DirTree a -> [a]
dirTreeExtraList = foldDirTree (\_ x -> x) concat (\_ e -> [e]) 

makeTreeRelative :: FilePath -> DirTree a -> DirTree a
makeTreeRelative path = mapDirTree (makeRelative path) ((,) . makeRelative path)

isEmpty :: DirTree a -> Bool
isEmpty = foldDirTree (\_ x -> x) and (const . const False)

treeFromDir :: FilePath -> IO (DirTree ())
treeFromDir dir = do
  tree <- treeFromDirAbs dir
  return $ makeTreeRelative dir tree

-- TODO: ignore files based on extension
ignoredPath :: FilePath -> Bool
ignoredPath path = elem path [".", "..", ".hvc", ".git", ".stack-work"]

treeFromDirAbs :: FilePath -> IO (DirTree ())
treeFromDirAbs dir = do
  let normDir = normalise dir
  contents <- getDirectoryContents normDir
  let filteredContents = filter (not . ignoredPath) contents
  nodes <- forM (sort filteredContents) $ \name -> do
    let path = normDir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then treeFromDirAbs path
      else return (FileNode path ())
  return (DirNode normDir nodes)
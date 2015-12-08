module DirTreeUtils 
       (treeFromDir
       ,emptyTreeDir
       ,extrasFromTree
       ,pathExtrasFromTree
       ,treeHash
       ,removeByteStrings
       ,HashContents
       ,hcHash
       ,hcContents) where

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import System.FilePath ((</>), makeRelative, normalise)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad (forM)
import Data.List

import DirTree
import Hash

type HashContents = (String, Lazy.ByteString)

hcHash :: HashContents -> String
hcHash = fst

hcContents :: HashContents -> Lazy.ByteString
hcContents = snd

removeByteStrings :: DirTree HashContents -> DirTree String
removeByteStrings = foldDirTree DirNode id (\p hc -> FileNode p (hcHash hc))

treeHash :: DirTree String -> String
treeHash tree = bstrToHex $ bstrSHA1 (foldr Strict.append Strict.empty pairHashes)
  where pairHashes = foldDirTree (\_ x -> x)
                     concat 
                     (\p h -> [strSHA1 (h ++ (bstrToHex $ strSHA1 p))])
                     tree

extrasFromTree :: DirTree a -> [a]
extrasFromTree = foldDirTree (\_ x -> x) concat (\_ e -> [e])

pathExtrasFromTree :: DirTree a -> [(FilePath, a)]
pathExtrasFromTree = foldDirTree (\_ x -> x) concat (\p e -> [(p, e)])

-- TODO: ignore files based on extension
ignoredPath :: FilePath -> Bool
ignoredPath path = elem path [".", "..", ".hvc", ".git", ".stack-work"]

makeTreeRelative :: FilePath -> DirTree a -> DirTree a
makeTreeRelative path =
  foldDirTree (DirNode . (makeRelative path))
              id
              (FileNode . (makeRelative path))

treeFromDir :: FilePath -> IO (DirTree HashContents)
treeFromDir dir = do
  tree <- treeFromDirAbs dir
  return $ makeTreeRelative dir tree

loadFileData :: FilePath -> IO HashContents
loadFileData path = do
  contents <- Lazy.readFile path
  let hash = bstrToHex $ lbstrSHA1 contents
  return (hash, contents)

treeFromDirAbs :: FilePath -> IO (DirTree HashContents)
treeFromDirAbs dir = do
  let normDir = normalise dir
  contents <- getDirectoryContents normDir
  let filteredContents = filter (not . ignoredPath) contents
  nodes <- forM (sort filteredContents) $ \name -> do
    let path = normDir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then treeFromDirAbs path
      else do
        fileData <- loadFileData path
        return (FileNode path fileData)
  return (DirNode normDir nodes)

emptyTreeDir :: DirTree String
emptyTreeDir = DirNode "." []
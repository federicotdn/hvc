module DirTree
       (DirTree(DirNode, FileNode)
       ,foldDirTree) where

data DirTree a = DirNode FilePath [DirTree a] | FileNode FilePath a deriving (Read, Show)

foldDirTree :: (FilePath -> c -> b) 
               -> ([b] -> c) 
               -> (FilePath -> a -> b) 
               -> DirTree a
               -> b
foldDirTree _g _k f (FileNode path extra) = f path extra
foldDirTree g k f (DirNode path dts) = g path (k (map (foldDirTree g k f) dts))
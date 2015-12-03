module Utils
       (hasHvcDir
       ,hvcDir
       ,printHvcDirError) where

import System.FilePath (combine, (</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import Control.Monad (forM)

hvcDir :: FilePath -> FilePath
hvcDir dir = combine dir ".hvc"

printHvcDirError :: IO ()
printHvcDirError = putStrLn "Unable to perform operation: HVC directory (.hvc) not found."

hasHvcDir :: FilePath -> IO Bool
hasHvcDir dir = do
  let dirList = [hvcDir dir, hvcDir dir </> "commits", hvcDir dir </> "objects"]
  dirsExist <- forM dirList doesDirectoryExist
  headExists <- doesFileExist $ hvcDir dir </> "HEAD"
  return (and $ headExists : dirsExist)
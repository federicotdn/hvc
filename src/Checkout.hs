module Checkout (checkoutHvc) where

import qualified Data.ByteString.Lazy as Lazy
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad (forM, forM_)
import Codec.Compression.GZip

import DirTreeUtils
import Utils

objectsExist :: String -> [String] -> IO Bool
objectsExist dir hashes = do
  filesExist <- forM hashes $ \hash -> doesFileExist $ dir </> hash
  return $ and filesExist

restoreObject :: String -> (FilePath, String) -> IO ()
restoreObject dir (filepath, hash) = do
  let targetPath = dir </> filepath
  compressedContents <- Lazy.readFile (objectsDir dir </> hash)
  createDirectoryIfMissing True (takeDirectory targetPath)
  Lazy.writeFile targetPath (decompress compressedContents)

restoreObjects :: String -> [(FilePath, String)] -> IO ()
restoreObjects dir objs = forM_ objs (restoreObject dir)

execCheckout :: FilePath -> String -> IO ()
execCheckout dir hash = do
  let commitFile = commitsDir dir </> hash
  commitExists <- doesFileExist commitFile
  if commitExists
    then do
      commit <- loadCommit commitFile
      hasObjects <- objectsExist (objectsDir dir) (extrasFromTree commit)
      if hasObjects
        then do
          restoreObjects dir (pathExtrasFromTree commit)
          storeCommitHead dir hash
          putStrLn $ "Checkout: checked out commit " ++ hash
        else putStrLn "Checkout: unable to checkout commit: missing objects."
    else putStrLn "Checkout: unable to checkout commit: commit not found."


checkoutHvc :: FilePath -> String -> IO ()
checkoutHvc dir hash = execIfHvc dir (execCheckout dir hash)
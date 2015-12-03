module Checkout (checkoutHvc) where

import qualified Data.ByteString.Lazy as Lazy
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad (forM, forM_)
import Codec.Compression.GZip

import Utils

loadCommitLines :: FilePath -> IO [CommitLine]
loadCommitLines path = do
  contents <- readFile path
  return $ map read (tail (lines contents))

objectsExist :: String -> [String] -> IO Bool
objectsExist dir hashes = do
  filesExist <- forM hashes $ \hash -> doesFileExist $ dir </> hash
  return $ and filesExist

restoreObject :: String -> CommitLine -> IO ()
restoreObject dir (CommitLine filepath hash) = do
  let targetPath = dir </> filepath
  compressedContents <- Lazy.readFile (objectsDir dir </> hash)
  createDirectoryIfMissing True (takeDirectory targetPath)
  Lazy.writeFile targetPath (decompress compressedContents)

restoreObjects :: String -> [CommitLine] -> IO ()
restoreObjects dir objs = forM_ objs (restoreObject dir)

execCheckout :: FilePath -> String -> IO ()
execCheckout dir hash = do
  let commitFile = commitsDir dir </> hash
  commitExists <- doesFileExist commitFile
  if commitExists
    then do
      commitLines <- loadCommitLines commitFile
      hasObjects <- objectsExist (objectsDir dir) (map (\(CommitLine _ h) -> h) commitLines)
      if hasObjects
        then do
          restoreObjects dir commitLines
          storeCommitHead dir hash
          putStrLn $ "Checkout: checked out commit " ++ hash
        else putStrLn "Checkout: unable to checkout commit: missing objects."
    else putStrLn "Checkout: unable to checkout commit: commit not found."


checkoutHvc :: FilePath -> String -> IO ()
checkoutHvc dir hash = execIfHvc dir (execCheckout dir hash)
module Commit (commitHvc) where

import qualified Data.ByteString.Lazy as Lazy
import Control.Monad (forM_)
import Codec.Compression.GZip
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.IO (withFile, hPutStrLn, IOMode(..))
import Data.Time (getCurrentTime)

import DirTreeUtils
import DirTree
import Utils

storeObject :: FilePath -> HashContents -> IO ()
storeObject dest hc = do
  let finalName = dest </> (hcHash hc)
  exists <- doesFileExist finalName
  if exists
    then return ()
    else Lazy.writeFile finalName (compress $ hcContents hc)

storeObjects :: FilePath -> [HashContents] -> IO ()
storeObjects base obs = do
  forM_ obs $ \hc -> storeObject (objectsDir base) hc

storeCommitData :: FilePath -> String -> String -> DirTree String -> IO ()
storeCommitData base msg hash hashesTree = do
  withFile (commitsDir base </> hash) WriteMode $ \file -> do
    date <- getCurrentTime
    hPutStrLn file (show $ CommitSummary msg (show date) hash)
    hPutStrLn file (show hashesTree)

storeCommit :: FilePath -> String -> DirTree HashContents -> IO String
storeCommit base msg tree = do
  let hashesTree = removeByteStrings tree
  let commitHash = treeHash hashesTree
  storeCommitHead base commitHash
  storeCommitData base msg commitHash hashesTree
  return commitHash

execCommit :: FilePath -> String -> IO ()
execCommit dir msg = do
  tree <- treeFromDir dir
  let hashContents = extrasFromTree tree
  if length hashContents == 0
    then putStrLn "Commit: no files to commit."
    else do
      storeObjects dir hashContents
      commitHash <- storeCommit dir msg tree
      putStrLn $ "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

commitHvc :: FilePath -> String -> IO ()
commitHvc dir msg = execIfHvc dir (execCommit dir msg)


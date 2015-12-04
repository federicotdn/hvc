module Commit (commitHvc) where

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Control.Monad (forM, forM_)
import Codec.Compression.GZip
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.IO (withFile, hPutStrLn, IOMode(..))
import Data.Time (getCurrentTime)

import DirTree
import Utils
import Hash

storeObject :: FilePath -> FilePath -> IO Strict.ByteString
storeObject obj dest = do
  contents <- Lazy.readFile obj
  let hashRaw = lbstrSHA1 contents
  let finalName = dest </> (bstrToHex hashRaw)
  exists <- doesFileExist finalName
  if exists
    then return hashRaw
    else do
      Lazy.writeFile finalName (compress contents)
      return hashRaw

storeObjects :: FilePath -> [FilePath] -> IO [Strict.ByteString]
storeObjects base obs = do
  forM obs $ \file -> do
    let filename = base </> file
    storeObject filename (objectsDir base)

commitHashFrom :: [(String, Strict.ByteString)] -> String
commitHashFrom fileHashes = bstrToHex $ bstrSHA1 (foldr Strict.append Strict.empty pairHashes)
  where pairHashes = map (\(file, hash) -> bstrSHA1 $ Strict.append (strSHA1 file) hash)
                         fileHashes

storeCommitData :: FilePath -> String -> String -> [(String, Strict.ByteString)] -> IO ()
storeCommitData base msg hash fileHashes = do
  withFile (commitsDir base </> hash) WriteMode $ \file -> do
    date <- getCurrentTime
    hPutStrLn file (show $ CommitSummary msg date hash)
    forM_ fileHashes $ \(filename, filehash) -> do
      hPutStrLn file (show $ CommitLine filename (bstrToHex filehash))

storeCommit :: FilePath -> String -> [(String, Strict.ByteString)] -> IO String
storeCommit base msg fileHashes = do
  let commitHash = commitHashFrom fileHashes
  storeCommitHead base commitHash
  storeCommitData base msg commitHash fileHashes
  return commitHash

execCommit :: FilePath -> String -> IO ()
execCommit dir msg = do
  tree <- treeFromDir dir
  let files = filesFromTree tree
  if length files == 0
    then putStrLn "Commit: no files to commit."
    else do
      hashes <- storeObjects dir files
      commitHash <- storeCommit dir msg (zip files hashes)
      putStrLn $ "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

commitHvc :: FilePath -> String -> IO ()
commitHvc dir msg = execIfHvc dir (execCommit dir msg)


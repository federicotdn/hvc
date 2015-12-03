module Commit (commitHvc) where

import Data.List
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Control.Monad (forM)
import Codec.Compression.GZip
import System.FilePath (combine, (</>))
import System.Directory (doesFileExist)

import DirTree
import Utils
import Hash

data CommitLine = CommitLine String String deriving (Show, Read)
data CommitSummary = CommitSummary String String deriving (Show, Read)

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
  forM (sort obs) $ \file -> do
    let filename = base </> file
    storeObject filename (hvcDir base </> "objects")

commitHashFrom :: [(String, Strict.ByteString)] -> String 
commitHashFrom fileHashes = bstrToHex $ bstrSHA1 (foldr Strict.append Strict.empty pairHashes)
  where pairHashes = map (\(file, hash) -> bstrSHA1 $ Strict.append (strSHA1 file) hash)
                         fileHashes

storeCommit :: FilePath -> [(String, Strict.ByteString)] -> IO String 
storeCommit base fileHashes = do
  return $ commitHashFrom fileHashes -- TODO: write commit data in commits dir, update HEAD

execCommit :: FilePath -> String -> IO ()
execCommit dir msg = do
  tree <- treeFromDir dir
  let files = filesFromTree tree
  if length files == 0
    then putStrLn "Commit: no files to commit."
    else do
      hashes <- storeObjects dir files
      commitHash <- storeCommit dir (zip files hashes)
      putStrLn $ "Commit hash: " ++ commitHash

commitHvc :: FilePath -> String -> IO ()
commitHvc dir msg = do
  hvcEnabled <- hasHvcDir dir
  if hvcEnabled
    then execCommit dir msg
    else printHvcDirError


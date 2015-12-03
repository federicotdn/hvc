module Init (initHvc) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>), combine)
import System.IO

import Utils

initHvc :: FilePath -> IO ()
initHvc dir = do
  exists <- doesDirectoryExist $ hvcDir dir
  if exists
    then putStrLn "Directory is already initialized."
    else do
      createDirectoryIfMissing True $ hvcDir dir </> "objects"
      createDirectoryIfMissing True $ hvcDir dir </> "commits"
      handle <- openFile (hvcDir dir </> "HEAD") WriteMode
      hPutStrLn handle ""
      hClose handle
      putStrLn $ "Directory: " ++ dir ++ " initialized."
module Init (initHvc) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import Utils

initHvc :: FilePath -> IO ()
initHvc dir = do
  exists <- doesDirectoryExist $ hvcDir dir
  if exists
    then putStrLn "Directory is already initialized."
    else do
      createDirectoryIfMissing True $ objectsDir dir
      createDirectoryIfMissing True $ commitsDir dir
      writeFile (headPath dir) "\n"
      putStrLn $ "Init: directory " ++ dir ++ " initialized."
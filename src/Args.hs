module Args
       (HvcArgsResult(..)
       ,HvcErrorType(..)
       ,HvcOperationType(..)
       ,parseArgs) where

import System.Directory

data HvcArgsResult = HvcError HvcErrorType | HvcOperation HvcOperationType
data HvcErrorType = DirError String
data HvcOperationType = Init String | Commit String | Help | Hash String String | Checkout String String

strToSimpleOp :: String -> String -> HvcOperationType
strToSimpleOp "commit" dir = Commit dir
strToSimpleOp "init" dir = Init dir
strToSimpleOp s _ = Help

strToCompoundOp :: String -> String -> String -> HvcOperationType
strToCompoundOp "hash" dir extra = Hash dir extra
strToCompoundOp "checkout" dir extra = Checkout dir extra
strToCompoundOp s _ _ = Help

validateCommand :: String -> String -> Maybe String -> IO HvcArgsResult
validateCommand command dir extra = do
  valid <- validDir dir
  if valid
  then case extra of
    Nothing -> return $ HvcOperation (strToSimpleOp command dir)
    Just e -> return $ HvcOperation (strToCompoundOp command dir e)
  else return $ HvcError (DirError dir)

parseArgs :: [String] -> IO HvcArgsResult
parseArgs [command, dir] = validateCommand command dir Nothing
parseArgs [command, dir, extra] = validateCommand command dir (Just extra)
parseArgs xs = return (HvcOperation Help)

validDir :: FilePath -> IO Bool
validDir fp = doesDirectoryExist fp
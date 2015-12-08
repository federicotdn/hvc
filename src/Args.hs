module Args
       (HvcArgsResult(..)
       ,HvcErrorType(..)
       ,HvcOperationType(..)
       ,parseArgs) where

import System.Directory

data HvcArgsResult = HvcError HvcErrorType | HvcOperation HvcOperationType
data HvcErrorType = DirError String
data HvcOperationType
  = Init String
  | Commit String String
  | Help
  | Checkout String String
  | Log String
  | Status String

strToSimpleOp :: String -> String -> HvcOperationType
strToSimpleOp "log" dir = Log dir
strToSimpleOp "init" dir = Init dir
strToSimpleOp "status" dir = Status dir
strToSimpleOp _ _ = Help

strToCompoundOp :: String -> String -> String -> HvcOperationType
strToCompoundOp "checkout" dir extra = Checkout dir extra
strToCompoundOp "commit" dir extra = Commit dir extra
strToCompoundOp _ _ _ = Help

strToOp :: String -> String -> Maybe String -> HvcOperationType
strToOp command dir (Nothing) = strToSimpleOp command dir
strToOp command dir (Just extra) = strToCompoundOp command dir extra

validateCommand :: String -> String -> Maybe String -> IO HvcArgsResult
validateCommand command dir extra =
  case operation of
    Help -> return (HvcOperation Help)
    _ -> do
      valid <- validDir dir
      if valid
        then return (HvcOperation operation)
        else return (HvcError $ DirError $ "Invalid directory: " ++ dir)
  where
    operation = strToOp command dir extra

parseArgs :: [String] -> IO HvcArgsResult
parseArgs ["help"] = return (HvcOperation Help)
parseArgs [dir, command] = validateCommand command dir Nothing
parseArgs [dir, command, extra] = validateCommand command dir (Just extra)
parseArgs _ = return (HvcOperation Help)

validDir :: FilePath -> IO Bool
validDir fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      permissions <- getPermissions fp
      return ((readable permissions) && (writable permissions))
    else return False
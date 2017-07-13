module FileWorker where

import System.Directory

inspectDir :: String -> IO [FilePath]
inspectDir path = do
  dt <- listDirectory path
  return dt
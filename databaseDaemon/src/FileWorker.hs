module FileWorker where

import System.IO
import System.Directory
import System.Exit

inspectDir :: String -> IO [FilePath]
inspectDir path = do
  dt <- listDirectory path
  return dt
import DatabaseFunctions
import DatabaseWorker
import FileWorker

import Data.Int
import Data.Char
import Data.List
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types
import System.IO
import System.Directory
import System.Exit
import System.Environment

main = do
  args <- getArgs
  
  let configurationFile = args !! 0
  let programPath = args !! 1
  
  settings <- getSettingsFromFile configurationFile
  conn <- connect settings
  
  internalFiles <- inspectDir programPath
  answers <- checkDatabaseRecord internalFiles conn
  let affected = foldl (\x y -> x + y) 0 answers
  print affected
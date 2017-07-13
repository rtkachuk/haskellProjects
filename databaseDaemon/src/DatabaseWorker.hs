module DatabaseWorker where

import DatabaseFunctions

import Data.Int
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types

checkDatabaseRecord :: [FilePath] -> Connection -> IO [Int64]
--checkDatabaseRecord files settings = addRecord (files !! 0) "Files" "Link" "Owner" settings
checkDatabaseRecord files settings = do
  sequence $ map (\n -> addRecord n "Files" "Link" "Owner" settings) files
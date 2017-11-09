module FileProcessing where

import System.IO
import System.Directory
import Control.Exception (evaluate)

databaseFile = "/home/fabler/db.txt"
serverVersion = "info:Server version 0.1.1"

checkDatabaseFile = do
  doesFileExist databaseFile >>= createDatabaseFile

createDatabaseFile True =
  putStrLn $ "File exist"

createDatabaseFile False = do
  putStrLn $ "File " ++ databaseFile ++ " created\n"
  writeFile databaseFile serverVersion 

writeDataToFile entry mode = do
  file <- openFile databaseFile mode
  hPutStr file entry
  hFlush file
  hClose file

getDataFromFile path = do
  file <- openFile path ReadMode
  text <- hGetContents file
  evaluate (length text)
  hClose file
  return text

module Main where

import System.IO
import System.Directory
import System.Environment
import System.Posix.Time
import System.Posix.Files
import Foreign.C.Types as CTypes
import Control.Applicative
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  checkArgs args
  let path =   args !! 0
      amount = (read (args !! 1)):: Int

  processEntries path amount

processEntries :: String -> Int -> IO()
processEntries path 0 = putStrLn $ "Done"
processEntries path amount = do

  fileNames <- listDirectory $ path
  let files = map (\file -> path ++ file) fileNames
  checkFilesExist files

  dates <- mapM (\file -> getFileLastChanged file >>= return) files

  let oldestDate = minimum dates
      filesToRemove = map fst $ filter (\(name, date) -> date == oldestDate) $ zip files dates
      amountOfFilesToRemove = length $ filesToRemove
      removeFilesWithPath = removeFiles path

  if (amountOfFilesToRemove < amount) then
    removeFilesWithPath filesToRemove $ amount - amountOfFilesToRemove
  else
    removeFilesWithPath (take amount filesToRemove) 0

checkArgs args
  | length args < 2 = putStrLn ("Usage: ./program path amount") >> exitFailure
  | otherwise = putStrLn $ "Scanning in " ++ args !! 0

checkFilesExist [] = putStrLn ("No files left") >> exitSuccess
checkFilesExist files = putStr $ ""

removeFiles path filesToRemove amount = do
    mapM_ (\file -> putStrLn $ "Removed " ++ file) filesToRemove
    mapM_ removeFile filesToRemove
    processEntries path amount

getFileLastChanged currentFile = do
  status <- getFileStatus currentFile
  let changedTime = modificationTime status
  return changedTime

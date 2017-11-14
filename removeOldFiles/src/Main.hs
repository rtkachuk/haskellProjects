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
  rawfiles <- listDirectory $ path
  let files = map (\file -> path ++ file) rawfiles
  checkFiles files
  dates <- mapM (\file -> getTime file >>= return) files
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

checkFiles [] = do
  putStrLn $ "No files left"
  exitSuccess
checkFiles files = putStr $ ""

removeFiles path filesToRemove amount = do
    mapM_ (\file -> putStrLn $ "Removed " ++ file) filesToRemove
    mapM_ removeFile filesToRemove
    processEntries path amount

getTime currentFile = do
  status <- getFileStatus currentFile
  let atime = modificationTime status
  return atime

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
  let path =   args !! 0
      amount = (read (args !! 1)):: Int

  processEntries path amount

processEntries :: String -> Int -> IO()
processEntries path 0 = putStrLn $ "Done"
processEntries path amount = do
  rawfiles <- listDirectory $ path
  let files = map (\file -> path ++ file) rawfiles
  checkFiles files
  filesAndDates <- mapM (\file -> file >>= return) $ generateFileAndDatesArray files []
  let (name,date) = head $ filesAndDates
      oldestDate = findOldest filesAndDates date
      filesToRemove = getFilesToRemove filesAndDates [] oldestDate
      amountOfFilesToRemove = length $ filesToRemove
  if amountOfFilesToRemove < amount then
    removeAll path filesToRemove $ amount - amountOfFilesToRemove
  else
    removeAll path (take amount filesToRemove) 0

checkFiles [] = do
  putStrLn $ "No files left"
  exitSuccess
checkFiles files = putStrLn $ show (length(files)) ++ " pending..."

removeAll path filesToRemove amount = do
  mapM_ (\file -> putStrLn $ "Removed " ++ file) filesToRemove
  mapM_ removeFile filesToRemove
  processEntries path amount

showFilesToBeRemoved :: [String] -> IO()
showFilesToBeRemoved [] = putStr $ ""
showFilesToBeRemoved files =
  putStrLn $ "Removed " ++ (head files)

getFilesToRemove :: [(String, CTypes.CTime)] -> [String] -> CTypes.CTime -> [String]
getFilesToRemove [] files time = files
getFilesToRemove filesToProcess filesToRemove time =
  let (name, date) = head $ filesToProcess
  in if date == time then do
    getFilesToRemove (tail filesToProcess) (filesToRemove ++ [name]) time
  else
    getFilesToRemove (tail filesToProcess) filesToRemove time

findOldest :: [(String, CTypes.CTime)] -> CTypes.CTime -> CTypes.CTime
findOldest [] time = time
findOldest filesWithDates time = do
  let (name, date) = head $ filesWithDates
  if date < time then
    findOldest (tail filesWithDates) date
  else
    findOldest (tail filesWithDates) time

showData :: [(String, CTypes.CTime)] -> IO()
showData [] = putStr $ "\n"
showData filesWithDates = do
  let (name, date) = head $ filesWithDates
  putStrLn $ name ++ (show date)
  showData $ tail filesWithDates

generateFileAndDatesArray :: [String] -> [IO (String, CTypes.CTime)] -> [IO (String, CTypes.CTime)]
generateFileAndDatesArray [] filesAndDates = filesAndDates
generateFileAndDatesArray files filesAndDates =
  let fileAndDate = getTime $ head files
  in generateFileAndDatesArray (tail files) $ fileAndDate : filesAndDates

getTime currentFile = do
  status <- getFileStatus currentFile
  let atime = modificationTime status
  return (currentFile, atime)

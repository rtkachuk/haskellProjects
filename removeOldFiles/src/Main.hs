module Main where

import System.IO
import System.Directory
import System.Environment
import System.Posix.Time
import System.Posix.Files
import Foreign.C.Types as CTypes
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  let path =   args !! 0
      amount = args !! 1

  files <- listDirectory $ path
  result <- mapM (\dat -> dat >>= return) (generateFileAndDatesArray path files [])
  showData result

showData :: [(String, CTypes.CTime)] -> IO()
showData [] = putStr $ "\n"
showData filesWithDates = do
  let (name, date) = head $ filesWithDates
  putStrLn $ name ++ (show date)
  showData $ tail filesWithDates

generateFileAndDatesArray :: String -> [String] -> [IO (String, CTypes.CTime)] -> [IO (String, CTypes.CTime)]
generateFileAndDatesArray path [] filesAndDates = filesAndDates
generateFileAndDatesArray path files filesAndDates =
  let fileAndDate = getTime $ path ++ "/" ++ (head files)
  in generateFileAndDatesArray path (tail files) (fileAndDate : filesAndDates)

getTime currentFile = do
  status <- getFileStatus currentFile
  let atime = modificationTime status
  return (currentFile, atime)

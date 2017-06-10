{-# LANGUAGE MultiWayIf #-}

import System.IO
import Control.Monad

type OpenedFileName = String

readFromFile :: OpenedFileName -> IO()
readFromFile file = do

-- Reading data from file, and printing it on the screen

  row <- readFile file
  print $ row

main = do

-- Let's open some file

  let fileName = "/home/fabler/test.txt"
  handle <- openFile fileName ReadMode
  print $ "Opening file: " ++ fileName

-- If file has been opened, let's read data from it. Otherwise,
-- exit program

  isOpened <- hIsOpen handle
  if | isOpened == True -> readFromFile fileName
     | otherwise -> print ("File is not opened!")

  hClose handle
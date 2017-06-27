{-# LANGUAGE LambdaCase #-}

import System.Directory
import System.Console.ANSI
import Control.Monad

getFiles :: [FilePath] -> IO()
getFiles contents = do
  isFile <- doesFileExist (head contents)
  if isFile == True then do
    print (head contents)
    getFiles (tail contents)
  else do
    getFiles (tail contents)

getFiles [] = print ""

searchPaths :: FilePath -> [FilePath] -> IO()
searchPaths path list = do
  contents <- listDirectory path
  let files = []
  getFiles contents
--  map (print) (files)
  
main = do
  path <- getCurrentDirectory
  print ("Current path: " ++ path)
  print ("==============================")
  searchPaths path []
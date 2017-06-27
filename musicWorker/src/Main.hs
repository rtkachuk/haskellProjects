{-# LANGUAGE LambdaCase #-}

import System.Directory
import System.Console.ANSI
import Control.Monad

getFiles :: FilePath -> [FilePath] -> IO()
getFiles path [] = return ()
getFiles path contents = do
  isFile <- doesDirectoryExist (head contents)
  if isFile == True then do
    if ((path !! ((length path)-1)) == '/') then
      searchPaths (path ++ (head contents))
    else
      searchPaths (path ++ "/" ++ (head contents))
    getFiles path (tail contents)
  else do
    print (path ++ ": " ++ (head contents))
    getFiles path (tail contents)

searchPaths :: FilePath -> IO()
searchPaths path = do
  contents <- listDirectory path
  getFiles path contents
  
main = do
  path <- getCurrentDirectory
  print ("Current path: " ++ path)
  print ("==============================")
  searchPaths path
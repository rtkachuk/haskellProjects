{-# LANGUAGE MultiWayIf #-}

import System.Directory
import System.Console.ANSI
import Control.Monad
import Data.List

checkFile :: FilePath -> IO()
checkFile file = do
  let checkNumber = (\n -> if ((((n !! 0) < '0') || ((n !! 0) > '9')) && (n !! 0) /= '-') then n else (tail n))
  let checkDot = (\n -> if (n !! 0 == '.') then (tail n) else n)
  let result = (checkDot (checkNumber (checkNumber (checkNumber file))))
  if file == result then
    print ("OK: " ++ file)
  else
    print ("Changed: " ++ file ++ " to " ++ result)

getFiles :: FilePath -> [FilePath] -> IO()
getFiles path [] = return ()
getFiles path contents = do
  isDir <- doesDirectoryExist (head contents)
  if
    | isDir == True -> do
      if ((path !! ((length path)-1)) == '/') then
        searchPaths (path ++ (head contents))
      else
        searchPaths (path ++ "/" ++ (head contents))
      getFiles path (tail contents)
    | otherwise -> do
      checkFile (head contents)
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
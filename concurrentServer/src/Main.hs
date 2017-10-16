{-# LANGUAGE MultiWayIf #-}

module Main where
import Network
import Control.Concurrent
import System.IO
import Data.List
import Data.String.Utils
import Control.Monad

logFile = "/home/fabler/log.txt"
tempFile = "/home/fabler/temp.txt"

main :: IO ()
main = withSocketsDo $ do
       appendFile logFile ""
       socket <- listenOn $ PortNumber 5002
       forever $ loop socket

loop sock = do
  (h,hn,pn) <- accept sock
  hSetBuffering h LineBuffering
  forkIO $ workWithSocket h

workWithSocket h = do
  message <- hGetLine $ h

  dataFromFile <- readFile logFile

  if | "GET " `isInfixOf` message -> do
          hPutStr h $ unlines $ seekData (lines dataFromFile) (drop 4 message) []
          hFlush h
     | "PUT " `isInfixOf` message  -> writeToFile dataFromFile $ (drop 4 message) ++ "\n"
     | otherwise -> hPutStr h "?command" >> hFlush h

  check <- hIsEOF h

  if check then
    hClose h
  else
    workWithSocket h

writeToFile :: String -> String -> IO()
writeToFile txt str = do
  let index = case findIndex (==':') str of
                Just a -> a
                otherwise -> 0
  let key = take index str
  let dat = drop (index+1) str

  processFile key dat txt

processFile key dat txt = do
  writeFile tempFile txt
  buff <- readFile tempFile
  writeFile logFile $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff
  appendFile logFile $ key ++ ":" ++ dat

seekData :: [String] -> String -> [String] -> [String]
seekData [] key result = result
seekData str key result = do
  let first = (head str)
  if | key `isInfixOf` first -> seekData (tail str) key (first : result)
     | otherwise -> seekData (tail str) key result
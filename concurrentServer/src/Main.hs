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
          let result =  unlines $ seekData (lines dataFromFile) (drop 4 message) []
          hPutStrLn h $ returnJson $ findData $ result
          hFlush h
     | "PUT " `isInfixOf` message  -> processEntry dataFromFile $ (drop 4 message) ++ "\n"
     | otherwise -> hPutStrLn h "?command" >> hFlush h

  check <- hIsEOF h

  if check then
    hClose h
  else
    workWithSocket h

returnJson :: String -> String
returnJson str = "{ data: \"" ++ filter (/= '\n') str ++ "\"}"

findKeySpacer :: String -> Int
findKeySpacer str = case findIndex (==':') str of
                      Just a -> a
                      otherwise -> 0

findKey :: String -> String
findKey str = take (findKeySpacer str) $ str

findData :: String -> String
findData str = drop ((findKeySpacer str) + 1) $ str

processEntry :: String -> String -> IO()
processEntry txt str = do
  let index = findKeySpacer str
  let key = findKey str
  let dat = findData str

  checkKey key dat txt

checkKey key dat txt = 
  if | length key == 0 -> return ()
     | otherwise -> writeToFile key dat txt


writeToFile key dat txt = do
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
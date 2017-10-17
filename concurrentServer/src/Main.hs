{-# LANGUAGE MultiWayIf #-}

module Main where
import Network
import Control.Concurrent
import System.IO
import Data.String.Utils
import Data.List
import Control.Monad

import Parsers

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
  
  if | "GET " `isInfixOf` message ->
          let len = length "GET "
          in sendToClient h $ returnJson $ findData $ seekData (lines dataFromFile) (drop len message)
     | "PUT " `isInfixOf` message  -> do
          let len = length "PUT "
          ans <- tryWrite dataFromFile $ (drop 4 message)
          sendToClient h ans
     | "DELETE " `isInfixOf` message -> do
          let len = length "DELETE "
          ans <- deleteEntry dataFromFile $ drop len message
          sendToClient h ans
     | "POST " `isInfixOf` message -> do
          let len = length "POST "
          ans <- processPost dataFromFile $ drop len message
          sendToClient h ans
     | otherwise -> hPutStrLn h "?command" >> hFlush h

  check <- hIsEOF h

  if check then
    hClose h
  else
    workWithSocket h

processPost dataFromFile received = do
  if | findKeySpacer received == 0 -> deleteEntry dataFromFile received
     | otherwise -> tryWrite dataFromFile received

sendToClient h msg =
  hPutStrLn h msg >> hFlush h

deleteEntry txt key = do
  writeFile tempFile txt
  buff <- readFile tempFile
  writeFile logFile $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff
  return $ "{\n\t\"action\":\"DEL\",\n\t\"state\":\"OK\",\n\t\"key\":\"" ++ key ++ "\"\n}"

tryWrite :: String -> String -> IO String
tryWrite txt str = do
  let index = findKeySpacer str
  let key = findKey str
  let dat = findData str

  if (keyIsValid key) then
    writeToFile key dat txt
  else
    return $ "{\n\t\"action\":\"WRITE\",\n\t\"state\":\"FAIL\",\n\t\"key\":\"" ++ key ++ "\"\n}"

writeToFile key dat txt = do
  writeFile tempFile txt
  buff <- readFile tempFile
  writeFile logFile $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff
  appendFile logFile $ key ++ ":" ++ dat ++ "\n"
  return $ "{\n\t\"action\":\"WRITE\",\n\t\"state\":\"OK\",\n\t\"key\":\"" ++ key ++ "\"\n}"

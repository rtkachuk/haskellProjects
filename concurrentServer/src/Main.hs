{-# LANGUAGE MultiWayIf #-}

module Main where
import Network
import Control.Concurrent
import System.IO
import Data.List

import Control.Monad

logFile = "/home/fabler/log.txt"

main :: IO ()
main = withSocketsDo $ do
       socket <- listenOn $ PortNumber 5002
       forever $ loop socket

loop sock = do
  (h,hn,pn) <- accept sock
  hSetBuffering h LineBuffering
  forkIO $ workWithSocket h

workWithSocket h = do
  message <- hGetLine $ h

  if | "GET " `isInfixOf` message -> do
          dataFromFile <- readFile logFile
          hPutStr h $ unlines $ seekData (lines dataFromFile) (drop 4 message) []
          hFlush h
     | otherwise -> writeToFile $ message ++ "\n"

  check <- hIsEOF h

  if check then
    hClose h
  else
    workWithSocket h

writeToFile str = do
  appendFile logFile str

seekData :: [String] -> String -> [String] -> [String]
seekData [] key result = result
seekData str key result = do
  let first = (head str)
  if | key `isInfixOf` first -> seekData (tail str) key (first : result)
     | otherwise -> seekData (tail str) key result
{-# LANGUAGE MultiWayIf #-}

module Main where

import Network
import Control.Concurrent
import System.IO
import System.Exit
import System.Posix.Process.ByteString

logFile = "/home/fabler/log.txt"

main :: IO ()
main = withSocketsDo $ do
       f <- openFile logFile AppendMode
       hSetBuffering f NoBuffering
       socket <- listenOn $ PortNumber 5002
       loop socket f

loop sock f = do
  (h,hn,pn) <- accept sock
  hSetBuffering h NoBuffering
  forkIO $ workWithSocket h f
  loop sock f

workWithSocket h f = do
  message <- hGetContents $ h
  let check = filter (/= '\n') message 
  if | check == "quit" -> do
                            hClose f
                            hClose h
                            exitImmediately ExitSuccess
     | otherwise -> do
                    hPutStr f message
  hClose h
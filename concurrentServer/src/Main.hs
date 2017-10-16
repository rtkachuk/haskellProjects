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
       f <- openFile logFile AppendMode
       hSetBuffering f LineBuffering
       mVar <- newEmptyMVar
       putMVar mVar f
       socket <- listenOn $ PortNumber 5002
       forever $ loop socket mVar

loop sock m = do
  (h,hn,pn) <- accept sock
  hSetBuffering h LineBuffering
  forkIO $ workWithSocket h m

workWithSocket h m = do
  message <- hGetLine $ h

  let var = "G" `isInfixOf` message
--  if | "G" `isInfixOf` message -> workWithMVar m "GET REQUEST"
--     | otherwise -> workWithMVar m message

  workWithMVar m message
  workWithMVar m $ show var
  check <- hIsEOF h

  if check then
    hClose h
  else
    workWithSocket h m

workWithMVar m str = do
  f <- takeMVar m
  hPutStr f str
  hFlush f
  putMVar m f
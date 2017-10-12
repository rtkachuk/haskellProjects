{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Strings

import System.Directory
import System.Console.ANSI

import Sound.ALUT

check::[Char]->[Char]->Bool
check [][]              =False
check _[]               =False
check []_               =False
check(x:xs)(y:ys)
 | y == x               =True -- this line
 | otherwise            =check xs (y:ys)

listSongs = do
  curr <- getCurrentDirectory
  cont <- listDirectory curr
  let songs = unlines $ filter (\x -> check x ".wav") $ cont
  if | strNull songs -> putStrLn $ "<no music>"
     | otherwise     ->  putStrLn songs
    
  checkState 3 ""

listDirs = do
  curr <- getCurrentDirectory
  cont <- listDirectory curr
  putStrLn $ unlines $ cont
  checkState 3 ""
  

cmmd = do
  putStrLn $ ">"
  cmd <- getLine
  clearScreen
  checkState 1 cmd

processCommand cmd = do
  let act = take 1 cmd
  case act of
    "p" -> checkState 2 (drop 2 cmd)
    "m" -> checkState 4 ""
    "l" -> checkState 5 ""
    "q" -> checkState 0 ""
    otherwise -> do 
      putStrLn $ "No such command"
      checkState 3 ""

showError err = do
  putStrLn $ err
  checkState 0 ""

playMusic cmd = do
      musicBuffer <- createBuffer $ File cmd
      musicSource <- genObjectName
      buffer musicSource $= Just musicBuffer
      play [musicSource]

      sleep 10

      {-let waitWhilePlaying = do
          sleep (0.1)
          state <- get (sourceState musicSource)
          if state == Playing then
            waitWhilePlaying
	  else
	    return putStrLn $ "Done"
      waitWhilePlaying-}

exit = putStrLn $ "Bye!"

checkState state cmd = 
  case state of 
    0 -> exit
    1 -> processCommand cmd
    2 -> playMusic cmd
    3 -> cmmd
    4 -> listSongs
    5 -> listDirs
    otherwise -> showError $ cmd

main :: IO ()
main = withProgNameAndArgs runALUT $ \_progName _args -> do
         putStrLn $ "Music Player v 0.1"
         checkState 3 ""

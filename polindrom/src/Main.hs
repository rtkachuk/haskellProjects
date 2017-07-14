{-# LANGUAGE MultiWayIf #-}

import System.Environment
import Data.Char

main = do
  args <- getArgs
  let word = map toLower $ filter isLetter $ head args
  if | word == reverse word -> putStrLn $ "OK!"
     | otherwise -> putStrLn $ "NO!"
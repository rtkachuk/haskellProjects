{-# LANGUAGE MultiWayIf #-}
import System.Environment

checkPolindrome :: String -> Bool
checkPolindrome str = do
  if | (length str > 1) -> do
                           if (head str) == (last str) then checkPolindrome (tail $ init $ str)
                           else False
     | otherwise -> True

main = do
  args <- getArgs
  if checkPolindrome (head args) == True then
    putStrLn ("OK!")
  else
    putStrLn ("NO!")
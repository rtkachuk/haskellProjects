{-# LANGUAGE MultiWayIf #-}

module Parsers where

import Data.String.Utils
import Data.List

keyIsValid key = 
  if | length key == 0 -> False
     | otherwise -> True

returnJson :: String -> String
returnJson "" = "{\n\t\"action\":\"READ\",\n\t\"state\":\"FAIL\",\n\t\"data\":\"<no data>\"\n}"
returnJson str = "{\n\t\"action\":\"READ\",\n\t\"state\":\"OK\",\n\t\"data\":\"" ++ init str ++ "\"\n}"

findKeySpacer :: String -> Int
findKeySpacer str = case findIndex (==':') str of
                      Just a -> a
                      otherwise -> 0

findKey :: String -> String
findKey str = take (findKeySpacer str) $ str

findData :: String -> String
findData str =
    let specialSymbolOffset = 1
    in drop ((findKeySpacer str) + specialSymbolOffset) $ str

seekData :: [String] -> String -> String
seekData [] key = ""
seekData str key = do
  let first = (head str)
  if | key `isInfixOf` first -> first
     | otherwise -> seekData (tail str) key
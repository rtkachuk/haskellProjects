{-# LANGUAGE MultiWayIf #-}

module Parsers where

import Data.String.Utils
import Data.List

findKeySpacer :: String -> Int
findKeySpacer str = case findIndex (==':') str of
                      Just a -> a
                      otherwise -> 0

getKey :: String -> String
getKey str = take (findKeySpacer str) $ str

getValue :: String -> String
getValue str =
    let specialSymbolOffset = 1
    in drop ((findKeySpacer str) + specialSymbolOffset) $ str

seekKeyValuePair :: [String] -> String -> String
seekKeyValuePair [] key = ""
seekKeyValuePair str key = do
  let first = (head str)
  if | key `isInfixOf` first -> first
     | otherwise -> seekKeyValuePair (tail str) key

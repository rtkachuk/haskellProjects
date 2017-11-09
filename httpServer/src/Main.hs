{-# LANGUAGE MultiWayIf #-}

module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Codec.Binary.UTF8.String
import System.IO
import Data.List
import Control.Concurrent.Lock as Lock

import Parsers
import JsonProcessing
import FileProcessing

main :: IO ()
main = do

  lock <- Lock.new

  checkDatabaseFileToBeExist

  serverWith defaultConfig { srvLog = stdLogger, srvPort=8888 } $ \_ url request ->
    with lock $ processRequest url request

processRequest url request = do
  let requestString = decodeString $ rqBody request
      urlString = url_path url
        
  case rqMethod request of

    GET -> processGet urlString
    POST -> processPost urlString requestString
    DELETE -> processDelete urlString

    _ -> return $ createJsonResponse "Not implemented" "" ""

--
-- Url is key, request is value
--

processGet key = sendReadRequest key

processPost key [] = sendReadRequest key
processPost key value = sendWriteRequest key value

processDelete key = do
  dataFromFile <- getDataFromFile databaseFile
  eraseKeyFromFile dataFromFile key

sendReadRequest key = do
  dataFromFile <- getDataFromFile databaseFile
  let value = getValue $ seekKeyValuePair (lines dataFromFile) $ key

  return $ createJsonResponse "Read" key value
  
sendWriteRequest key value = do
  fileContents <- getDataFromFile databaseFile
  eraseKeyFromFile fileContents key
  writeDataToFile ( key ++ ":" ++ value ++ "\n" ) AppendMode

  return $ createJsonResponse "Wrote" key value

eraseKeyFromFile fileContents key = do
  writeDataToFile ( unlines $ filter (\str -> not $ (key ++ ":") `isInfixOf` str) $ lines fileContents ) WriteMode

  return $ createJsonResponse "Erased" key ""

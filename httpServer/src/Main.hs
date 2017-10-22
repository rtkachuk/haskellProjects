{-# LANGUAGE MultiWayIf #-}

module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception(try, SomeException)
import System.FilePath
import Text.XHtml
import Text.JSON
import Text.JSON.String(runGetJSON)
import Data.List

import Parsers

dbFile = "/home/fabler/db.txt"
tempFile = "/tmp/HaskellHttpServerTempData.txt"

main :: IO ()
main = serverWith defaultConfig { srvLog = stdLogger, srvPort=8888 } $ \_ url request -> do

  dataFromFile <- readFile dbFile
  
  let req = decodeString $ rqBody request
      ur = url_path url
  case rqMethod request of

    GET -> readRequest ur dataFromFile

    POST -> processPost ur req dataFromFile
    
    DELETE -> processDelete ur dataFromFile

    _ -> return $ prepareHtml BadRequest $ toHtml "Not implemented"

readRequest url dataFromFile = do
  let text = findData $ seekData (lines dataFromFile) $ url

  if | length text == 0 -> sendJSONNoData
     | otherwise -> sendJSON text

writeRequest ur req dataFromFile = do
  ans <- tryWrite ur req dataFromFile
  return $ ans

generateJSONObject status text = do
  let dataObject = ("data", JSString $ toJSString $ text)
      statusObject = ("result", JSString $ toJSString $ status)

  return $ JSObject $ toJSObject [statusObject , dataObject ]

sendJSON text = do
  obj <- generateJSONObject "OK" text
  return $ prepareJSON OK $ obj

sendJSONNoData = do
  obj <- generateJSONObject "FAIL" "<no data>"
  return $ prepareJSON NotFound $ obj

sendText s v = insertHeader HdrContentLength (show (length v)) $
               insertHeader HdrContentEncoding "UTF-8" $
               insertHeader HdrContentEncoding "text/plain" $
               (respond s :: Response String) { rspBody = txt }
    where txt = encodeString v

prepareHtml :: StatusCode -> Html -> Response String
prepareHtml s v = insertHeader HdrContentType "text/html" $ sendText s $ renderHtml v

prepareJSON :: StatusCode -> JSValue -> Response String
prepareJSON s v = insertHeader HdrContentType "application/json" $ sendText s $ showJSValue v ""

processPost ur req dataFromFile = do
  if | length req == 0 -> readRequest ur dataFromFile
     | otherwise -> writeRequest ur req dataFromFile

processDelete ur dataFromFile = do
  writeFile tempFile dataFromFile
  buff <- readFile tempFile
  writeFile dbFile $ unlines $ filter (\str -> (ur ++ ":") `isInfixOf` str == False) $ lines buff
  ans <- sendJSON "REMOVED"
  return $ ans

tryWrite key dat txt = do
  if (keyIsValid key) then
    writeToFile key dat txt
  else do
    ans <- sendJSONNoData
    return $ ans

writeToFile key dat txt = do
  writeFile tempFile txt
  buff <- readFile tempFile
  writeFile dbFile $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff
  appendFile dbFile $ key ++ ":" ++ dat ++ "\n"
  ans <- sendJSON dat
  return $ ans

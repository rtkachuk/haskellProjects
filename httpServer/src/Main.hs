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

import Parsers

dbFile = "/home/fabler/log.txt"

main :: IO ()
main = serverWith defaultConfig { srvLog = stdLogger, srvPort=8888 } $ \_ url request -> do
  dataFromFile <- readFile dbFile
  case rqMethod request of

    GET -> do
      let text = findData $ seekData (lines dataFromFile) $ url_path $ url

      if | length text == 0 -> sendJSONNoData
         | otherwise -> sendJSON text
    _ -> return $ prepareHtml BadRequest $ toHtml "Not implemented"

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
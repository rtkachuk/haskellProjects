 {-# LANGUAGE MultiWayIf #-}

module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception(try, SomeException)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forever)
import System.FilePath
import System.IO
import Text.XHtml
import Text.JSON
import Text.JSON.String(runGetJSON)
import Data.List
import Control.Exception (evaluate)

import Parsers

dbFile = "/home/fabler/db.txt"
tempFile = "/tmp/HaskellHttpServerTempData.txt"

data ChannelMode = CGet | CPostRead | CPostWrite | CDelete

main :: IO ()
main = do
  actionChannel <- newChan
  getChannel <- newChan
  postReadChannel <- newChan

  worker $ writerListener actionChannel getChannel postReadChannel
  serverWith defaultConfig { srvLog = stdLogger, srvPort=8888 } $ \_ url request -> do

    let req = decodeString $ rqBody request
        ur = url_path url
    case rqMethod request of

      GET -> do getRequest ur actionChannel getChannel

      POST -> processPost ur req actionChannel postReadChannel
    
      DELETE -> processDelete ur actionChannel

      _ -> return $ prepareHtml BadRequest $ toHtml "Not implemented"

--
-- Worker. It actually starts forever threads for reading/writing
--

worker action = forkIO $ forever action

--
-- Chanel, which will have acsess to file
--

writerListener actionChannel getChannel postReadChannel= do
  dat <- readChan actionChannel
  processChannelRecv dat getChannel postReadChannel

processChannelRecv (mode, dat) getChannel postReadChannel =
  case mode of
    CGet -> do
      dataFromFile <- getDataFromFile dbFile
      let text = findData $ seekData (lines dataFromFile) $ dat
      writeChan getChannel text
    CPostRead -> do
      dataFromFile <- getDataFromFile dbFile
      let text = findData $ seekData (lines dataFromFile) $ dat
      writeChan postReadChannel text
    CPostWrite -> do
      txt <- getDataFromFile dbFile
      let key = findKey dat
      let str = findData dat
      writeDataToFile tempFile txt
      buff <- getDataFromFile tempFile
      writeDataToFile dbFile $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff
      appendDataToFile dbFile $ key ++ ":" ++ str ++ "\n"
    CDelete -> do
      txt <- getDataFromFile dbFile
      let key = dat
      writeDataToFile tempFile txt
      buff <- getDataFromFile tempFile
      writeDataToFile dbFile $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff

getDataFromFile path = do
  file <- openFile path ReadMode
  text <- hGetContents file
  evaluate (length text)
  hClose file
  return text

writeDataToFile path dat = do
  file <- openFile path WriteMode
  hPutStr file dat
  hFlush file
  hClose file

appendDataToFile path dat = do
  file <- openFile path AppendMode
  hPutStr file dat
  hFlush file
  hClose file

--
--  Read (GET) processing
--

getRequest url actionChannel readChannel = do

  writeChan actionChannel (CGet, url)
  text <- readChan readChannel

  sendReadResult text

postReadRequest url actionChannel readChannel = do
  writeChan actionChannel (CPostRead, url)
  text <- readChan readChannel

  sendReadResult text

sendReadResult text =
  if | length text == 0 -> do
        ans <- sendJSONNoData
        return ans
     | otherwise -> do
        ans <- sendJSON text
        return ans

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


--
-- Write processing
--

writeRequest ur req dataFromFile = do
  ans <- tryWrite ur req dataFromFile
  return $ ans

sendText s v = insertHeader HdrContentLength (show (length v)) $
               insertHeader HdrContentEncoding "UTF-8" $
               insertHeader HdrContentEncoding "text/plain" $
               (respond s :: Response String) { rspBody = txt }
    where txt = encodeString v

prepareHtml :: StatusCode -> Html -> Response String
prepareHtml s v = insertHeader HdrContentType "text/html" $ sendText s $ renderHtml v

prepareJSON :: StatusCode -> JSValue -> Response String
prepareJSON s v = insertHeader HdrContentType "application/json" $ sendText s $ showJSValue v ""

processPost ur req actionChannel postReadChannel = do
  if | length req == 0 -> postReadRequest ur actionChannel postReadChannel
     | otherwise -> writeRequest ur req actionChannel

processDelete ur actionChannel = do
  writeChan actionChannel (CDelete, ur)
  ans <- sendJSON "REMOVED"
  return $ ans

tryWrite key dat actionChannel = do
  if (keyIsValid key) then do
    writeChan actionChannel (CPostWrite, (key ++ ":" ++ dat))
    ans <- sendJSON "OK"
    return $ ans
  else do
    ans <- sendJSONNoData
    return $ ans

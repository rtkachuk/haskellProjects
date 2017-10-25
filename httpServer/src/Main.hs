{-# LANGUAGE MultiWayIf #-}

module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Concurrent
import Control.Monad (forever)
import System.FilePath
import System.IO
import System.Directory
import Text.XHtml
import Text.JSON
import Data.List
import Control.Exception (evaluate)

import Parsers

db_FILE = "/home/fabler/db.txt"
tempFILE = "/tmp/HaskellHttpServerTempData.txt"

serverVERSION = "Server version 0.0.1"

data ChannelMode = CGet | CPostRead | CPostWrite | CDelete

main :: IO ()
main = do
  actionChannel <- newChan
  getChannel <- newChan
  postChannel <- newChan

  exist <- doesFileExist db_FILE
  if | exist -> putStrLn $ "File exist"
     | otherwise -> writeFile db_FILE serverVERSION

  worker $ writerListener actionChannel getChannel postChannel
  serverWith defaultConfig { srvLog = stdLogger, srvPort=8888 } $ \_ url request -> do

    let req = decodeString $ rqBody request
        ur = url_path url
        
    case rqMethod request of

      GET -> do getRequest ur actionChannel getChannel

      POST -> processPost ur req actionChannel postChannel
    
      DELETE -> processDelete ur actionChannel

      _ -> return $ prepareHtml BadRequest $ toHtml "Not implemented"

--
-- Worker. It actually starts forever threads for reading/writing
--

worker action = forkIO $ forever action

--
-- Chanel, which will have acsess to file
--

writerListener actionChannel getChannel postChannel= do
  dat <- readChan actionChannel
  processChannelRecv dat getChannel postChannel

processChannelRecv (mode, dat) getChannel postChannel =
  case mode of
    CGet -> do
      dataFromFile <- getDataFromFile db_FILE
      let text = findData $ seekData (lines dataFromFile) $ dat
      writeChan getChannel text
    CPostRead -> do
      dataFromFile <- getDataFromFile db_FILE
      let text = findData $ seekData (lines dataFromFile) $ dat
      writeChan postChannel text
    CPostWrite -> do
      txt <- getDataFromFile db_FILE
      let key = findKey dat
      let str = findData dat
      writeDataToFile tempFILE txt
      buff <- getDataFromFile tempFILE
      writeDataToFile db_FILE $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff
      appendDataToFile db_FILE $ key ++ ":" ++ str ++ "\n"
    CDelete -> do
      txt <- getDataFromFile db_FILE
      let key = dat
      writeDataToFile tempFILE txt
      buff <- getDataFromFile tempFILE
      writeDataToFile db_FILE $ unlines $ filter (\str -> (key ++ ":") `isInfixOf` str == False) $ lines buff

--
--  File working. Theese functions were reimplemented
--  due to avoid file-locking.
--

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

--
-- Read (POST) processing
--

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

--
-- Generating JSON objects
--

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
-- Write processing (POST only)
--

processPost ur req actionChannel postChannel = do
  if | length req == 0 -> postReadRequest ur actionChannel postChannel
     | otherwise -> writeRequest ur req actionChannel

writeRequest ur req dataFromFile = do
  ans <- tryWrite ur req dataFromFile
  return $ ans

tryWrite key dat actionChannel = do
  if (keyIsValid key) then do
    writeChan actionChannel (CPostWrite, (key ++ ":" ++ dat))
    ans <- sendJSON "OK"
    return $ ans
  else do
    ans <- sendJSONNoData
    return $ ans

--
-- Process Delete request
--

processDelete ur actionChannel = do
  writeChan actionChannel (CDelete, ur)
  ans <- sendJSON "REMOVED"
  return $ ans

--
-- Prepare "Not implemented" error page
--

prepareHtml :: StatusCode -> Html -> Response String
prepareHtml s v = insertHeader HdrContentType "text/html" $ sendText s $ renderHtml v

--
-- Prepare JSON object
--

prepareJSON :: StatusCode -> JSValue -> Response String
prepareJSON s v = insertHeader HdrContentType "application/json" $ sendText s $ showJSValue v ""

sendText s v = insertHeader HdrContentLength (show (length v)) $
               insertHeader HdrContentEncoding "UTF-8" $
               insertHeader HdrContentEncoding "text/plain" $
               (respond s :: Response String) { rspBody = txt }
    where txt = encodeString v
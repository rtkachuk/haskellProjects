module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception(try, SomeException)
import System.FilePath
import Text.XHtml

main :: IO ()
main = serverWith defaultConfig { srvLog = stdLogger, srvPort=8888 } $ \_ url request ->
  case rqMethod request of

    GET -> do
        check <- try $ readFile $ url_path $ url
        case check of
          Right dat -> return $ prepareHtml OK $ primHtml dat
          Left exept -> do
            text <- readFile $ "404.html"
            return $ prepareHtml NotFound $ primHtml $ text
            where _hack :: SomeException
                  _hack = exept

    _ -> return $ prepareHtml BadRequest $ toHtml "Not implemented"

sendText s v = insertHeader HdrContentLength (show (length v)) $
               insertHeader HdrContentEncoding "UTF-8" $
               insertHeader HdrContentEncoding "text/plain" $
               (respond s :: Response String) { rspBody = txt }
    where txt = encodeString v

prepareHtml :: StatusCode -> Html -> Response String
prepareHtml s v = insertHeader HdrContentType "text/html" $ sendText s $ renderHtml v
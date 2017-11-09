module JsonProcessing where

import Network.HTTP.Server
import Codec.Binary.UTF8.String
import Text.JSON

createJsonResponse :: String -> String -> String -> Response String
createJsonResponse status key value = do
  let statusObject = ("result", JSString $ toJSString $ status)
      keyObject = ("Key", JSString $ toJSString $ key)
      valueObject = ("Value", JSString $ toJSString $ value)
      
      jsonObject = JSObject $ toJSObject [ statusObject , keyObject , valueObject ]
      
  generateResponseHeader OK $ showJSValue jsonObject ""

generateResponseHeader :: StatusCode -> String -> Response String
generateResponseHeader status value =
               insertHeader HdrContentType "application/json" $
               insertHeader HdrContentLength (show (length value)) $
               insertHeader HdrContentEncoding "UTF-8" $
               insertHeader HdrContentEncoding "text/plain" $
               (respond status :: Response String) { rspBody = responseBody }
    where responseBody = encodeString value

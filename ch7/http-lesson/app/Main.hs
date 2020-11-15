module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Lib

myToken :: BC.ByteString
myToken = "xSXleecbBZFCiWsykReWJqKmNJrLKzwf"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
                                $ setRequestHost host
                                $ setRequestHeader "token" [token]
                                $ setRequestPath path
                                $ setRequestSecure True
                                $ setRequestPort 443
                                $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

main :: IO ()
main = do
    response <- httpLBS request
    if getResponseStatusCode response == 200
        then do
            print "saving request to file"
            let jsonBody = getResponseBody response
            L.writeFile "data.json" jsonBody
        else do
            print "request failed with error"
            print $ getResponseStatus response

-- q39.1
buildRequestNoSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNoSSL token host method path = setRequestMethod method
                                $ setRequestHost host
                                $ setRequestHeader "token" [token]
                                $ setRequestPath path
                                $ setRequestSecure False
                                $ setRequestPort 443
                                $ defaultRequest


module Main where

import Lib
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status


myToken :: BC.ByteString
myToken = "sdDmmPGijmvhSsAZPJHjVFESIoHpJvQH"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString 
             -> BC.ByteString 
             -> BC.ByteString
             -> BC.ByteString
             -> Request
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
    let ourStatusCode = getResponseStatusCode response
    let ourStatus = getResponseStatus response
    -- let msg = status statusCode
    if ourStatusCode == 200
    then do 
         print "saving request to file"
         print (statusCode ourStatus)
         print (statusMessage ourStatus)
         let jsonBody = getResponseBody response
         L.writeFile "data.json" jsonBody
    else print (mconcat [ "request failed with error: " 
                        , (show (statusCode ourStatus))
                        ," "
                        ,BC.unpack (statusMessage ourStatus)])



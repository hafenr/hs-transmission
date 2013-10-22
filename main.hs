{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as BS8
import Control.Monad (liftM)


import Network.Stream
import Network.HTTP

import Network
import GHC.IO.Handle
import System.IO

import Data.Aeson

import Types

data TorrentPage = TorrentPage { title :: String
                               , url :: String
                               -- function to get ids or whatever
                               -- the thing that the torrent lib understands
                               , getIds :: [String]
                               } deriving (Show, Eq)

kickassTo = TorrentPage { title = "kickass.to"
                        , url = "http://www.kickass.to"
                        , getIds = kickassIds }
    where kickassIds = ["1"]

urls =
    [ "http://kickass.to/hourlydump.txt.gz" ]

main :: IO ()
main = do
    entries <- readZippedText $ "hourlydump.txt.gz"
    putStrLn "adsf"

readZippedText :: FilePath -> IO [String]
readZippedText fname =
    liftM (lines . BS8.unpack . GZip.decompress) $ BS8.readFile fname

portNr = 9090

netTest = do
    -- sock <- listenOn (PortNumber portNr)
    -- (h, host, port) <- accept sock
    -- hPutStr h "Hallo :)\n"


    conn <- connectTo "localhost" (PortNumber portNr)
    hSetBuffering conn NoBuffering

    let json = "{\
                \   \"arguments\": {\
                \       \"fields\": [ \"id\", \"name\", \"totalSize\" ],\
                \       \"ids\": [ 1 ]\
                \   },\
                \   \"method\": \"torrent-get\"\
                \}"


    hPutStr conn json

    hFlush conn
    putStrLn "flushed"

    putStrLn "recieving...."
    answer <- hGetChar conn

    -- threadDelay 1000

    putChar answer

    hClose conn


transmissionPort = 9095
testjson = "{\"method\": \"torrent-get\", \"arguments\": {\"ids\": [1], \"fields\" : [\"name\"]}}"
transhttp = "http://localhost:" ++ show transmissionPort ++ "/transmission/rpc"
testRequest = postRequestWithBody transhttp "application/json" testjson


test = do
    result <- simpleHTTP testRequest
    putStrLn $ show result
    -- situation von "monads are not composable"?
    -- result >>= return . getTransmissionId >>= simpleHttp etc
    let maybeId = case result of
                       Right response -> extractSessionId response
    result2 <- case maybeId of
                    Just id -> simpleHTTP (testRequest `withSessionId` id)
    let body = case result2 of
                    Right response -> rspBody response
    return body

    -- return newReq `fmap` maybeId


executeRPC json =
    let request = postRequestWithBody transhttp "application/json" json
    in makeRequest request

-- main method, requests have to be build be the user
-- makeRequest :: RpcRequest -> RpcResponse
-- several convenience functions like

-- convenience functionm for accessor requests

-- getTorrentField :: [TorrentField] IdArgument -> Response

-- setTorrentField :: [MutatorArgument] Id

-- actionTorrent ::

--

makeRequest request = do
    result <- simpleHTTP request
    answerBody <-
        case result of -- extract all the error handlich stuff in some function handleHttpError and maybe also one for handleTransmissionError (like 409)
        -- these functions could also check the result for errors and, if ok, return result. otherwise handle error
             Left ErrorReset -> fail "ErrorReset encountered"
             Left ErrorClosed -> fail "ErrorClosed encountered"
             Left (ErrorParse errorMsg) -> fail $ "ErrorParse " ++ errorMsg
             Left (ErrorMisc errorMsg) -> fail $ "ErrorMisc " ++ errorMsg
             Right response @ (Response code reason headers body) ->
                 case code of
                      (4,0,9) -> case (extractSessionId response) of
                                      Just sessionId -> makeRequest (request `withSessionId` sessionId)
                                      Nothing -> fail "SHAZBOT"
                      (2,0,0) -> return body
                      otherwise -> fail "SHAZBOT, HTTP return code unknown hierchen"
    return answerBody

    -- case JSON.decode answerBody of
    --      JSON.Ok json -> return json
    --      JSON.Error message -> fail $ "JsonError " ++ message

-- handleHttpError err = ...


-- handleTransmissionError err ...

---- hello message to get session id
-- getSessionId

request `withSessionId` sessionId =
    let newHeader = Header (HdrCustom "X-Transmission-Session-Id") sessionId
        updatedRqHeaders = newHeader : rqHeaders request
    in request {rqHeaders = updatedRqHeaders}


extractSessionId :: Response String -> Maybe String
extractSessionId (Response code reason headers body) =
    lookupHeader (HdrCustom "X-Transmission-Session-Id") headers


{-# LANGUAGE OverloadedStrings #-}

module Main where

-- DATA
import Discogs.Types.Release.Community
import Discogs.Types.Search

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (readFile, split)
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

discogs :: String -> IO ByteString
discogs s = do
    initReq <- parseUrl s
    man <- newManager tlsManagerSettings
    let req = initReq { requestHeaders = [("User-Agent","dummytesting")] } -- , ("Authorization", "Discogs token=wtMdvtQFtCvaizjUGxxeNRmJuWjQwwLktwTwPfsE")
    resp <- httpLbs req man
    return $ responseBody resp

getInput :: IO [ByteString]
getInput = LBS.split (BS.c2w '\n') <$> LBS.readFile "input.txt"

main :: IO ()
main = undefined

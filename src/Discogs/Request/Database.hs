{-# LANGUAGE OverloadedStrings #-}

module Discogs.Request.Database where

import Discogs.Types.Search

import Prelude hiding (concat)
import Data.Text hiding (concat, pack, append)
import Network.HTTP.Client
import Data.Default.Class
import Network.HTTP.Types
import Data.ByteString hiding (pack)
import Data.ByteString.Char8 (pack)

toBS :: Show a => a -> ByteString
toBS = pack . show

getRelease :: Int -> Request
getRelease n = def { path = append "/releases/" $ toBS n }

getArtist :: Int -> Request
getArtist n = def { path = append "/artists/" $ toBS n }

getMaster :: Int -> Request
getMaster n = def { path = append "/masters/" $ toBS n }

getLabel :: Int -> Request
getLabel n = def { path = append "/labels/" $ toBS n }

getReleaseVersions :: Int -> Request
getReleaseVersions n = def { path = concat ["/masters/", toBS n, "/versions"] }

getArtistReleases :: Int -> Request
getArtistReleases n = def { path = concat ["/artists/", toBS n, "/releases"] }

getLabelReleases :: Int -> Request
getLabelReleases n = def { path = concat ["/labels/", toBS n, "/releases"] }

search :: SearchQuery -> Request
search query = def { path = concat ["/database/search?", toBS $ prepareQuery query ] }

{-# LANGUAGE OverloadedStrings #-}

module Discogs.Request.Database where

import Discogs.Tools

import Prelude hiding (concat)
import Network.HTTP.Client
import Data.ByteString hiding (pack)

getRelease :: Int -> Request
getRelease n = secureReq { path = append "/releases/" $ intToBs n }

getArtist :: Int -> Request
getArtist n = secureReq { path = append "/artists/" $ intToBs n }

getMaster :: Int -> Request
getMaster n = secureReq { path = append "/masters/" $ intToBs n }

getLabel :: Int -> Request
getLabel n = secureReq { path = append "/labels/" $ intToBs n }

getReleaseVersions :: Int -> Request
getReleaseVersions n = secureReq { path = concat ["/masters/", intToBs n, "/versions"] }

getArtistReleases :: Int -> Request
getArtistReleases n = secureReq { path = concat ["/artists/", intToBs n, "/releases"] }

getLabelReleases :: Int -> Request
getLabelReleases n = secureReq { path = concat ["/labels/", intToBs n, "/releases"] }

search :: Params -> Request
search ps = setQueryString (mkParams ps) r
            where
            r = secureReq { path = "/database/search" }

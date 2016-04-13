{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

-- DATA
import Discogs.Login
import Discogs.Types.Error
import Discogs.Types.Discogs
import Discogs.Types.Release
import Discogs.Types.Artist
import Discogs.Types.Label

import qualified Discogs.Request.Database as RDB
import qualified Discogs.Request.MarketPlace as RMP

import Discogs.Actions.Database
import Discogs.Actions.MarketPlace

import Discogs.Tools

import Data.Maybe (fromJust)
import Data.Aeson
import Data.Text hiding (append)
import Data.Default.Class
import Control.Monad
import Control.Exception
import Data.ByteString.Lazy (ByteString, append, toStrict)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Free

tok :: ByteString
tok = "gdnfNCVuNkjWFzhFBQkZkSOIAPHlmCDwMYnsOAsN"

main :: IO ()
main = undefined

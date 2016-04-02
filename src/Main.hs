{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

-- DATA
import Discogs.Login
import Discogs.Types.Error
import Discogs.Types.Discogs
import Discogs.Actions.Database
import Discogs.Types.Release
import Discogs.Types.Artist
import Discogs.Types.Label

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

-- testProgram :: (MonadIO m, Monad m) => DiscogsT m ()
-- testProgram = do
--     r <- release 1
--     liftIO $ print $ title r
--     ar <- artist 1
--     liftIO $ print $ Discogs.Types.Artist.name ar
--     l <- label 1
--     liftIO $ print $ Discogs.Types.Label.name l
--     sr <- search [("name", "ben sims")]
--     liftIO $ print sr
--     return ()

main :: IO ()
main = undefined

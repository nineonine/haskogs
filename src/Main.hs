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

runDiscogs :: (MonadIO m, FromJSON a) => ByteString -> DiscogsT m a -> m (Either DiscogsError a)
runDiscogs token = runDiscogsWith def { loginMethod = Token token }

runDiscogsAnon :: (MonadIO m, FromJSON a) => DiscogsT m a -> m (Either DiscogsError a)
runDiscogsAnon  = runDiscogsWith def

runDiscogsWith  :: (MonadIO m, FromJSON a) => DiscogsOptions -> DiscogsT m a -> m (Either DiscogsError a)
runDiscogsWith (DiscogsOptions rl man lm ua) discogs = do
    manager <- case man of
        Just m -> return m
        Nothing -> liftIO $ newManager tlsManagerSettings
    userAgent <- case ua of
        Just s -> return s
        Nothing -> return "discogs-haskell 0.1.0.0 by nineonine"
    let headers = [("User-Agent", toStrict userAgent)]
    headers' <- if  lm /= Anonymous
                    then return $ ("Authorization", toStrict $ append "Discogs token=" (getToken lm)) : headers
                    else return headers
    interpretIO (def { connMgr = Just manager , usrAgent = userAgent , extraHeaders = headers'}) discogs


interpretIO :: (MonadIO m, FromJSON a) => DiscogsState -> DiscogsT m a -> m (Either DiscogsError a)
interpretIO state@(DiscogsState baseUrl rl mgr headers creds ua ) (DiscogsT r) = runFreeT r >>= \case
      Pure x -> return $ Right x
      Free (RunRequest r next) -> do
          let r' = r { requestHeaders = headers, host = toStrict baseUrl }
          Just respObject <- liftIO $ (decode . responseBody) <$> httpLbs r' (fromJust mgr)
          interpretIO state $ DiscogsT $ next respObject

testProgram :: (MonadIO m, Monad m) => DiscogsT m ()
testProgram = do
    r <- release 1
    liftIO $ print $ title r
    ar <- artist 1
    liftIO $ print $ Discogs.Types.Artist.name ar
    l <- label 1
    liftIO $ print $ Discogs.Types.Label.name l
    sr <- search [("name", "ben sims")]
    liftIO $ print sr
    return ()

main :: IO ()
main = undefined

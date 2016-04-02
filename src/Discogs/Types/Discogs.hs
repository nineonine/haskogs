{-# LANGUAGE OverloadedStrings, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase#-}

module Discogs.Types.Discogs where

import Discogs.Login
import Discogs.Types.Error

import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Data.ByteString.Lazy (ByteString, append, toStrict)
import Data.Default.Class

data DiscogsF (m :: * -> *) (a :: *) where
    RunRequest :: FromJSON b => Request -> (b -> a) -> DiscogsF m a
    -- ProcessResponse :: FromJSON b => Request -> (b -> a) -> DiscogsF m a

instance Functor (DiscogsF m) where
    fmap f (RunRequest r x) = RunRequest r (f . x)
    -- fmap f (ProcessResponse r x) = ProcessResponse r (f . x)

newtype DiscogsT m a = DiscogsT (FreeT (DiscogsF m) m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans DiscogsT where
    lift = DiscogsT . lift

instance (MonadIO m) => MonadIO (DiscogsT m) where
    liftIO = DiscogsT . liftIO

runRequest :: (FromJSON a, Monad m) => Request -> DiscogsT m a
runRequest r = DiscogsT $ liftF $ RunRequest r id

-- processResponse :: (FromJSON a, Monad m) => Request -> DiscogsT m a
-- processResponse r = DiscogsT $ liftF $ ProcessResponse r id

data DiscogsOptions = DiscogsOptions
    { enableRateLimit   :: Bool
    , connectionManager :: Maybe Manager
    , loginMethod       :: LoginMethod
    , customUserAgent   :: Maybe ByteString }

instance Default DiscogsOptions where
    def = DiscogsOptions True Nothing Anonymous Nothing

data DiscogsState = DiscogsState
    { baseUrl         :: ByteString
    , rateLimit       :: Bool
    , connMgr         :: Maybe Manager
    , extraHeaders    :: [Header]
    , credentials     :: LoginMethod
    , usrAgent        :: ByteString }

instance Default DiscogsState where
    def = DiscogsState "api.discogs.com" True Nothing [] Anonymous "discogs-haskell 0.1.0.0 by nineonine"

runDiscogs :: (MonadIO m, FromJSON a) => ByteString -> DiscogsT m a -> m (Either DiscogsError a)
runDiscogs token = runDiscogsWith def { loginMethod = Token token }

runDiscogsAnon :: (MonadIO m, FromJSON a) => DiscogsT m a -> m (Either DiscogsError a)
runDiscogsAnon  = runDiscogsWith def

runDiscogsWith  :: (MonadIO m, FromJSON a) => DiscogsOptions -> DiscogsT m a -> m (Either DiscogsError a)
runDiscogsWith (DiscogsOptions _ man lm ua) discogs = do
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
interpretIO state@(DiscogsState url _ mgr headers _ _ ) (DiscogsT r) = runFreeT r >>= \case
      Pure x -> return $ Right x
      Free (RunRequest req next) -> do
          let r' = req { requestHeaders = headers, host = toStrict url }
          Just respObject <- liftIO $ (decode . responseBody) <$> httpLbs r' (fromJust mgr)
          interpretIO state $ DiscogsT $ next respObject

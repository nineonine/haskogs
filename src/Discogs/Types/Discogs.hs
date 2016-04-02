{-# LANGUAGE OverloadedStrings, GADTs, GeneralizedNewtypeDeriving, KindSignatures #-}

module Discogs.Types.Discogs where

import Discogs.Login

import Data.Text
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.ByteString.Lazy (ByteString)
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

{-# LANGUAGE OverloadedStrings, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase #-}

module Discogs.Types.Discogs where

import Discogs.Tools
import Discogs.Login
import Discogs.Types.Error

import Data.Maybe
import Data.Aeson
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Data.ByteString.Lazy (ByteString, append, toStrict)
import Data.Default.Class

data DiscogsF (m :: * -> *) next where
    RunRequest :: (FromJSON a) => Request -> (a -> next) -> DiscogsF m next

instance Functor (DiscogsF m) where
    fmap f (RunRequest r g) = RunRequest r (f . g)

newtype DiscogsT m a = DiscogsT (FreeT (DiscogsF m) m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans DiscogsT where
    lift = DiscogsT . lift

instance (MonadIO m) => MonadIO (DiscogsT m) where
    liftIO = DiscogsT . liftIO

runRequest :: (FromJSON a, Monad m) => Request -> DiscogsT m a
runRequest r = DiscogsT $ liftF $ RunRequest r id

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
        Nothing -> return "haskogs 0.1.0.0 by nineonine"
    let headers = [("User-Agent", toStrict userAgent)]
    headers' <- if  lm /= Anonymous
                    then return $ ("Authorization", toStrict $ append "Discogs token=" (getToken lm)) : headers
                    else return headers
    interpretIO (def { connMgr = Just manager , usrAgent = userAgent , extraHeaders = headers'}) discogs

interpretIO :: (MonadIO m) => DiscogsState -> DiscogsT m a -> m (Either DiscogsError a)
interpretIO state@(DiscogsState url _ mgr headers _ _ ) (DiscogsT r) = runFreeT r >>= \case
    Pure x -> return $ Right x
    Free (RunRequest req next) -> do
        -- case method req of
        let r' = req { requestHeaders = requestHeaders req ++ headers
                     , host = toStrict url
                     }
        liftIO $ print $ r'
        -- liftIO $ print $ requestBody r'
        result <- liftIO $ try $ httpLbs r' (fromJust mgr)
        case result of
            Left e -> return $ Left $ Some e
            Right response -> do
                liftIO $ print $ responseStatus response
                let (Just respObject) = (decode . responseBody) response
                interpretIO state $ DiscogsT $ next respObject



-- interpretIO state $ DiscogsT $ next

withParams :: (MonadIO m) => DiscogsT m a -> Params -> DiscogsT m a
withParams (DiscogsT r) params = DiscogsT $ transFreeT ( `withParams'` params) r

withParams' :: DiscogsF m a -> Params ->  DiscogsF m a
withParams' (RunRequest r next) params = flip RunRequest next $ setQueryString (mkParams params) r

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
    def = DiscogsState "api.discogs.com" True Nothing [] Anonymous "haskogs 0.1.0.0 by nineonine"

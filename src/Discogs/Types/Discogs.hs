{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# LANGUAGE KindSignatures, LambdaCase , RankNTypes #-}

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
    HandleResponse :: (FromJSON a) => Response ByteString -> (a -> next) -> DiscogsF m next
    FailWith :: DiscogsError -> DiscogsF m next

instance Functor (DiscogsF m) where
    fmap f (RunRequest r g) = RunRequest r (f . g)
    fmap f (HandleResponse r n) = HandleResponse r (f . n)
    fmap _ (FailWith e) = FailWith e

newtype DiscogsT m a = DiscogsT (FreeT (DiscogsF m) m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans DiscogsT where
    lift = DiscogsT . lift

instance (MonadIO m) => MonadIO (DiscogsT m) where
    liftIO = DiscogsT . liftIO

runRequest :: (FromJSON a, Monad m) => Request -> DiscogsT m a
runRequest r = DiscogsT . liftF $ RunRequest r id

handleResponse :: (FromJSON a, Monad m) => Response ByteString -> DiscogsT m a
handleResponse r = DiscogsT . liftF $ HandleResponse r id

failWith  :: (Monad m) => DiscogsError -> DiscogsT m a
failWith e = DiscogsT . liftF $ FailWith e

runDiscogs :: (MonadIO m, FromJSON a) => ByteString -> DiscogsT m a -> m (Either DiscogsError (Maybe a))
runDiscogs token = runDiscogsWith def { loginMethod = Token token }

runDiscogsAnon :: (MonadIO m, FromJSON a) => DiscogsT m a -> m (Either DiscogsError (Maybe a))
runDiscogsAnon  = runDiscogsWith def

runDiscogsWith  :: (MonadIO m, FromJSON a) => DiscogsOptions -> DiscogsT m a -> m (Either DiscogsError (Maybe a))
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

interpretIO :: (MonadIO m) => DiscogsState -> DiscogsT m a -> m (Either DiscogsError (Maybe a))
interpretIO state@(DiscogsState url _ mgr headers _ _ ) (DiscogsT r) = runFreeT r >>= \case
    Pure x -> return $ Right $ Just x
    Free (FailWith e) -> return $ Left e
    Free (RunRequest req next) -> do
        let r' = req { requestHeaders = requestHeaders req ++ headers , host = toStrict url }
        maybeResponse <- liftIO $ try $ httpLbs r' (fromJust mgr)
        case maybeResponse of
            Left e -> interpretIO state . DiscogsT . wrap . FailWith $ Some e
            Right resp -> interpretIO state . DiscogsT . wrap $ HandleResponse resp next
    Free (HandleResponse rsp next) ->
        case statusCode $ responseStatus rsp of
            404 -> interpretIO state . DiscogsT . wrap . FailWith . HTTPError $ responseStatus rsp
            204 -> return $ Right Nothing
            _   -> case eitherDecode $ responseBody rsp of
                       Left s -> interpretIO state . DiscogsT . wrap . FailWith $ ParseError s
                       Right a -> interpretIO state . DiscogsT $ next a


withParams :: (MonadIO m) => DiscogsT m a -> Params -> DiscogsT m a
withParams (DiscogsT r) params = DiscogsT $ transFreeT ( `withParams'` params) r

withParams' :: DiscogsF m a -> Params ->  DiscogsF m a
withParams' (RunRequest r next) params = flip RunRequest next $ setQueryString (mkParams params) r
withParams' _ _                        = FailWith $ DiscogsError BadParamsUsageError

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

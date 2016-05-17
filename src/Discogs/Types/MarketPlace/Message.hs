{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.MarketPlace.Message where

import           Discogs.Types.Alias
import           Discogs.Types.Pagination
import           Discogs.Types.MarketPlace.Refund

import           Data.Aeson
import           Data.Text
import           Data.Time.Clock
import           GHC.Generics hiding (from)

data Messages = Messages
    { pagination :: Maybe Pagination
    , messages   :: [Message]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Message = Message
    { refund    :: Maybe Refund
    , from      :: Maybe Alias
    , actor     :: Maybe Alias
    , original  :: Maybe Int
    , new       :: Maybe Int
    , timestamp :: UTCTime
    , message   :: Text
    , mtype     :: Maybe Text
    , order     :: OrderResource
    , subject   :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Message where
    parseJSON = withObject "message" $ \o -> do
        refund    <- o .:? "refund"
        from      <- o .:? "from"
        actor     <- o .:? "actor"
        original  <- o .:? "original"
        new       <- o .:? "new"
        timestamp <- o .: "timestamp"
        message   <- o .: "message"
        mtype     <- o .:? "type"
        order     <- o .: "order"
        subject   <- o .: "subject"
        return Message{..}

instance ToJSON Message where
    toJSON Message{..} = object [
        "refund"    .= refund,
        "from"      .= from,
        "actor"     .= actor,
        "original"  .= original,
        "new"       .= new,
        "timestamp" .= timestamp,
        "message"   .= message,
        "type"      .= mtype,
        "order"     .= order,
        "subject"   .= subject ]

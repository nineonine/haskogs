{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Marketplace.Message where

import           Discogs.Tools
import           Discogs.Types.Alias
import           Discogs.Types.Pagination
import           Discogs.Types.Marketplace.Refund

import           Data.Aeson
import           Data.Text
import           Data.Time.Clock

data Messages = Messages
    { msgs_pagination :: Maybe Pagination
    , msgs_messages   :: [Message]
    } deriving (Show, Read, Eq)

instance Paginated Messages where
    type Content Messages = [Message]
    pagination = msgs_pagination
    contents = msgs_messages

instance FromJSON Messages where
    parseJSON = withObject "searchResult" $ \o -> do
        msgs_pagination <- o.:? "pagination"
        msgs_messages   <- o .: "messages"
        return Messages{..}

instance ToJSON Messages where
    toJSON Messages{..} = object [
        "pagination" .= msgs_pagination ,
        "messages"    .= msgs_messages ]

data Message = Message
    { message_refund    :: Maybe Refund
    , message_from      :: Maybe Alias
    , message_actor     :: Maybe Alias
    , message_original  :: Maybe Int
    , message_new       :: Maybe Int
    , message_timestamp :: UTCTime
    , message_message   :: Text
    , message_type      :: Maybe Text
    , message_order     :: OrderResource
    , message_subject   :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Message where
    parseJSON = withObject "message" $ \o -> do
        message_refund    <- o .:? "refund"
        message_from      <- o .:? "from"
        message_actor     <- o .:? "actor"
        message_original  <- o .:? "original"
        message_new       <- o .:? "new"
        message_timestamp <- o .: "timestamp"
        message_message   <- o .: "message"
        message_type      <- o .:? "type"
        message_order     <- o .: "order"
        message_subject   <- o .: "subject"
        return Message{..}

instance ToJSON Message where
    toJSON Message{..} = object [
        "refund"    .= message_refund,
        "from"      .= message_from,
        "actor"     .= message_actor,
        "original"  .= message_original,
        "new"       .= message_new,
        "timestamp" .= message_timestamp,
        "message"   .= message_message,
        "type"      .= message_type,
        "order"     .= message_order,
        "subject"   .= message_subject ]

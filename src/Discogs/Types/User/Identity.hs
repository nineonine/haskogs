{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.Identity where

import Data.Text
import Data.Aeson

data Identity = Identity
    { resource_url  :: Text
    , _id           :: Int
    , username      :: Text
    , consumer_name :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Identity where
    parseJSON = withObject "identity" $ \o -> do
        resource_url  <- o .: "resource_url"
        _id           <- o .: "id"
        username      <- o .: "username"
        consumer_name <- o .: "consumer_name"
        return Identity{..}

instance ToJSON Identity where
    toJSON Identity{..} = object [
        "resource_url"  .= resource_url,
        "id"            .= _id,
        "username"      .= username,
        "consumer_name" .= consumer_name ]

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.Folder where

import Data.Text hiding (count)
import Data.Aeson

data Folder = Folder
    { count        :: Int
    , resource_url :: Text
    , name         :: Text
    , _id          :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Folder where
    parseJSON = withObject "folder" $ \o -> do
        count        <- o .: "count"
        resource_url <- o .: "resource_url"
        name         <- o .: "name"
        _id          <- o .: "id"
        return Folder{..}

instance ToJSON Folder where
    toJSON Folder{..} = object [
        "count"      .= count,
        "name"         .= name,
        "resource_url" .= resource_url,
        "id"           .= _id ]

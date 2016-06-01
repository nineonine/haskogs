{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.Folder where

import Data.Text hiding (count)
import Data.Aeson
import Discogs.Tools

data Folder = Folder
    { folder_count        :: Int
    , folder_resource_url :: Text
    , folder_name         :: Text
    , folder_id           :: Int
    } deriving (Show, Read, Eq)

instance DiscogsResource Folder where
    type ID Folder = Int
    resourceId = folder_id
    resourceUrl = folder_resource_url

instance FromJSON Folder where
    parseJSON = withObject "folder" $ \o -> do
        folder_count        <- o .: "count"
        folder_resource_url <- o .: "resource_url"
        folder_name         <- o .: "name"
        folder_id           <- o .: "id"
        return Folder{..}

instance ToJSON Folder where
    toJSON Folder{..} = object [
        "count"        .= folder_count,
        "name"         .= folder_name,
        "resource_url" .= folder_resource_url,
        "id"           .= folder_id ]

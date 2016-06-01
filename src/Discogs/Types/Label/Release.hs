{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Label.Release where

import Discogs.Types.Pagination

import Data.Text
import Data.Aeson
import Discogs.Tools

data LabelRelease = LabelRelease
    { lr_status       :: Text
    , lr_thumb        :: Text
    , lr_title        :: Text
    , lr_format       :: Text
    , lr_catno        :: Text
    , lr_year         :: Maybe Int
    , lr_resource_url :: Text
    , lr_artist       :: Text
    , lr_id           :: Int
    } deriving (Show, Read, Eq)

instance DiscogsResource LabelRelease where
    type ID LabelRelease = Int
    resourceId = lr_id
    resourceUrl = lr_resource_url

instance FromJSON LabelRelease where
    parseJSON = withObject "release" $ \o -> do
        lr_status <- o .: "status"
        lr_thumb <- o .: "thumb"
        lr_title <- o .: "title"
        lr_format <- o .: "format"
        lr_catno <- o .: "catno"
        lr_year <- o .:? "year"
        lr_resource_url <- o .: "resource_url"
        lr_artist <- o .: "artist"
        lr_id <- o .: "id"
        return LabelRelease{..}

instance ToJSON LabelRelease where
    toJSON LabelRelease{..} = object [
        "status" .= lr_status,
        "thumb" .= lr_thumb,
        "title" .= lr_title,
        "format" .= lr_format,
        "catno" .= lr_catno,
        "year" .= lr_year,
        "resource_url" .= lr_resource_url,
        "artist" .= lr_artist,
        "id" .= lr_id ]


data LabelReleases = LabelReleases
    { lr_pagination :: Maybe Pagination
    , lr_releases   :: [LabelRelease]
    } deriving (Show, Read, Eq)

instance Paginated LabelReleases where
    type Content LabelReleases = [LabelRelease]
    pagination = lr_pagination
    contents = lr_releases

instance FromJSON LabelReleases where
    parseJSON = withObject "LabelReleases" $ \o -> do
        lr_pagination <- o.:? "pagination"
        lr_releases   <- o .: "releases"
        return LabelReleases{..}

instance ToJSON LabelReleases where
    toJSON LabelReleases{..} = object [
        "pagination" .= lr_pagination,
        "releases" .= lr_releases ]

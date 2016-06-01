{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Release.Versions where

import           Discogs.Tools
import           Discogs.Types.Pagination

import           Data.Aeson
import           Data.Text
import           GHC.Generics


data ReleaseVersion = ReleaseVersion
    { rv_status       :: Text
    , rv_thumb        :: Text
    , rv_format       :: Text
    , rv_title        :: Text
    , rv_label        :: Text
    , rv_released     :: Text
    , rv_catno        :: Text
    , rv_resource_url :: Text
    , rv_id           :: Int
    } deriving (Show, Read, Eq)

instance DiscogsResource ReleaseVersion where
    type ID ReleaseVersion = Int
    resourceId = rv_id
    resourceUrl = rv_resource_url

instance FromJSON ReleaseVersion where
    parseJSON = withObject "release" $ \o -> do
        rv_status <- o .: "status"
        rv_thumb <- o .: "thumb"
        rv_format <- o .: "format"
        rv_title <- o .: "title"
        rv_label <- o .: "label"
        rv_released <- o .: "released"
        rv_catno <- o .: "catno"
        rv_resource_url <- o .: "resource_url"
        rv_id <- o .: "id"
        return ReleaseVersion{..}

instance ToJSON ReleaseVersion where
    toJSON ReleaseVersion{..} = object [
        "status" .= rv_status,
        "thumb" .= rv_thumb,
        "format" .= rv_format,
        "title" .= rv_title,
        "label" .= rv_label,
        "released" .= rv_released,
        "catno" .= rv_catno,
        "resource_url" .= rv_resource_url,
        "id" .= rv_id ]

data Versions = Versions
    { versions_pagination :: Maybe Pagination
    , versions_versions   :: [ReleaseVersion]
    } deriving (Show, Read, Eq)

instance Paginated Versions where
    type Content Versions = [ReleaseVersion]
    pagination = versions_pagination
    contents = versions_versions

instance FromJSON Versions where
    parseJSON = withObject "searchResult" $ \o -> do
        versions_pagination <- o.:? "pagination"
        versions_versions   <- o .: "versions"
        return Versions{..}

instance ToJSON Versions where
    toJSON Versions{..} = object [
        "pagination" .= versions_pagination ,
        "versions"   .= versions_versions ]

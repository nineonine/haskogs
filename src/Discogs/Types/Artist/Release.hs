{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Artist.Release where

import Discogs.Types.Pagination

import Data.Text
import Data.Aeson

data ArtistRelease = ArtistRelease
    { ar_thumb        :: Text
    , ar_artist       :: Text
    , ar_main_release :: Maybe Int
    , ar_title        :: Text
    , ar_role         :: Text
    , ar_year         :: Maybe Int
    , ar_resource_url :: Text
    , ar_release_type :: Maybe Text
    , ar_id           :: Int
    } deriving (Show, Read, Eq)

instance FromJSON ArtistRelease where
    parseJSON = withObject "artist" $ \o -> do
        ar_thumb <- o .: "thumb"
        ar_artist <- o .: "artist"
        ar_main_release <- o .:? "main_release"
        ar_title <- o .: "title"
        ar_role <- o .: "role"
        ar_year <- o .:? "year"
        ar_resource_url <- o .: "resource_url"
        ar_release_type <- o .:? "release_type"
        ar_id <- o .: "id"
        return ArtistRelease{..}

instance ToJSON ArtistRelease where
    toJSON ArtistRelease{..} = object [
        "thumb" .= ar_thumb ,
        "artist" .= ar_artist ,
        "main_release" .= ar_main_release ,
        "title" .= ar_title ,
        "role" .= ar_role ,
        "year" .= ar_year ,
        "resource_url" .= ar_resource_url ,
        "release_type" .= ar_release_type ,
        "id" .= ar_id ]

data ArtistReleases = ArtistReleases
    { ar_pagination :: Maybe Pagination
    , ar_releases   :: [ArtistRelease]
    } deriving (Show, Read, Eq)

instance FromJSON ArtistReleases where
    parseJSON = withObject "searchResult" $ \o -> do
        ar_pagination <- o.:? "pagination"
        ar_releases   <- o .: "releases"
        return ArtistReleases{..}

instance ToJSON ArtistReleases where
    toJSON ArtistReleases{..} = object [
        "pagination" .= ar_pagination ,
        "releases"   .= ar_releases ]

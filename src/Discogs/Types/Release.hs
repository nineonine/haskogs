{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Release where

import           Discogs.Types.Image
import           Discogs.Types.Release.Artist
import           Discogs.Types.Release.Community
import           Discogs.Types.Release.Company
import           Discogs.Types.Release.Format
import           Discogs.Types.Release.Identifier
import           Discogs.Types.Release.Label
import           Discogs.Types.Release.Track
import           Discogs.Types.Release.Video

import           Control.Applicative
import           Data.Aeson
import           Data.Text

data Release = Release
    { release_styles             :: Maybe [Text]
    , release_videos             :: Maybe [Video]
    , release_series             :: [ReleaseLabel]
    , release_released_formatted :: Text
    , release_labels             :: [ReleaseLabel]
    , release_community          :: Community
    , release_year               :: Int
    , release_images             :: [Image]
    , release_format_quantity    :: Int
    , release_id                 :: Int
    , release_genres             :: [Text]
    , release_thumb              :: Text
    , release_extraartists       :: Maybe [ReleaseArtist]
    , release_title              :: Text
    , release_artists            :: [ReleaseArtist]
    , release_date_changed       :: Text
    , release_master_id          :: Maybe Int
    , release_tracklist          :: Maybe [Track]
    , release_status             :: Text
    , release_estimated_weight   :: Maybe Int
    , release_master_url         :: Maybe Text
    , release_released           :: Text
    , release_date_added         :: Text
    , release_country            :: Maybe Text
    , release_notes              :: Maybe Text
    , release_identifiers        :: [Identifier]
    , release_companies          :: [Company]
    , release_uri                :: Text
    , release_formats            :: [Format]
    , release_resource_url       :: Text
    , release_data_quality       :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Release where
    parseJSON = withObject "release" $ \o -> do
        release_styles             <- optional (o .: "styles")
        release_videos             <- optional (o .: "videos")
        release_series             <- o .: "series"
        release_released_formatted <- o .: "released_formatted"
        release_labels             <- o .: "labels"
        release_community          <- o .: "community"
        release_year       <- o .: "year"
        release_images     <- o .: "images"
        release_format_quantity    <- o .: "format_quantity"
        release_id         <- o .: "id"
        release_genres             <- o .: "genres"
        release_thumb      <- o .: "thumb"
        release_extraartists       <- optional ( o .: "extraartists" )
        release_title      <- o .: "title"
        release_artists            <- o .: "artists"
        release_date_changed       <- o .: "date_changed"
        release_master_id          <- optional (o .: "master_id")
        release_tracklist          <- optional (o .: "tracklist")
        release_status             <- o .: "status"
        release_estimated_weight   <- optional (o .: "estimated_weight" )
        release_master_url         <- optional (o .: "master_url")
        release_released           <- o .: "released"
        release_date_added         <- o .: "date_added"
        release_country            <- optional (o .: "country")
        release_notes              <- optional (o .: "notes")
        release_identifiers        <- o .: "identifiers"
        release_companies          <- o .: "companies"
        release_uri                <- o .: "uri"
        release_formats            <- o .: "formats"
        release_resource_url   <- o .: "resource_url"
        release_data_quality   <- o .: "data_quality"
        return Release{..}

instance ToJSON Release where
    toJSON Release{..} = object [
        "styles" .= release_styles,
        "videos" .= release_videos,
        "series" .= release_series,
        "released_formatted" .= release_released_formatted,
        "labels" .= release_labels,
        "community" .= release_community,
        "year" .= release_year,
        "images" .= release_images,
        "format_quantity" .= release_format_quantity,
        "id" .= release_id,
        "genres" .= release_genres,
        "thumb" .= release_thumb,
        "extraartists" .= release_extraartists,
        "title" .= release_title,
        "artists" .= release_artists,
        "date_changed" .= release_date_changed,
        "master_id" .= release_master_id,
        "tracklist" .= release_tracklist,
        "status" .= release_status,
        "estimated_weight" .= release_estimated_weight,
        "master_url" .= release_master_url,
        "released" .= release_released,
        "date_added" .= release_date_added,
        "country" .= release_country,
        "notes" .= release_notes,
        "identifiers" .= release_identifiers,
        "companies" .= release_companies,
        "uri" .= release_uri,
        "formats" .= release_formats,
        "resource_url" .= release_resource_url,
        "data_quality" .= release_data_quality ]

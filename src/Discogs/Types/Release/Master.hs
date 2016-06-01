{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Release.Master where

import Discogs.Types.Release.Artist
import Discogs.Types.Release.Video
import Discogs.Types.Release.Track
import Discogs.Types.Image

import Data.Text
import Data.Aeson
import Discogs.Tools

data Master = Master
      { master_styles             :: [Text]
      , master_main_release       :: Int
      , master_main_release_url   :: Text
      , master_resource_url       :: Text
      , master_versions_url       :: Text
      , master_videos             :: Maybe [Video]
      , master_year               :: Int
      , master_images             :: [Image]
      , master_id                 :: Int
      , master_genres             :: [Text]
      , master_title              :: Text
      , master_artists            :: [ReleaseArtist]
      , master_tracklist          :: [Track]
      , master_uri                :: Text
      , master_data_quality       :: Text
      } deriving (Show, Read, Eq)

instance DiscogsResource Master where
    type ID Master = Int
    resourceId = master_id
    resourceUrl = master_resource_url

instance FromJSON Master where
    parseJSON = withObject "master" $ \o -> do
        master_styles  <- o .: "styles"
        master_main_release  <- o .: "main_release"
        master_main_release_url  <- o .: "main_release_url"
        master_resource_url  <- o .: "resource_url"
        master_versions_url  <- o .: "versions_url"
        master_videos  <- o .:? "videos"
        master_year  <- o .: "year"
        master_images  <- o .: "images"
        master_id  <- o .: "id"
        master_genres  <- o .: "genres"
        master_title  <- o .: "title"
        master_artists  <- o .: "artists"
        master_tracklist  <- o .: "tracklist"
        master_uri  <- o .: "uri"
        master_data_quality  <- o .: "data_quality"
        return Master{..}

instance ToJSON Master where
    toJSON Master{..} = object [
        "styles" .= master_styles,
        "main_release" .= master_main_release ,
        "main_release_url" .= master_main_release_url ,
        "resource_url" .= master_resource_url ,
        "versions_url" .= master_versions_url ,
        "videos" .= master_videos ,
        "year" .= master_year ,
        "images" .= master_images ,
        "id" .= master_id ,
        "genres" .= master_genres ,
        "title" .= master_title ,
        "artists" .= master_artists ,
        "tracklist" .= master_tracklist ,
        "uri" .= master_uri ,
        "data_quality" .= master_data_quality ]

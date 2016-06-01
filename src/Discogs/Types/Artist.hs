{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Artist where

import           Discogs.Tools
import           Discogs.Types.Alias
import           Discogs.Types.Artist.Member
import           Discogs.Types.Image

import           Control.Applicative
import           Data.Aeson
import           Data.Text

data Artist = Artist
    { artist_profile        :: Text
    , artist_releases_url   :: Text
    , artist_name           :: Text
    , artist_realname       :: Maybe Text
    , artist_groups         :: Maybe [Member]
    , artist_uri            :: Text
    , artist_members        :: Maybe [Member]
    , artist_urls           :: Maybe [Text]
    , artist_images         :: Maybe [Image]
    , artist_resource_url   :: Text
    , artist_aliases        :: Maybe [Alias]
    , artist_id             :: Int
    , artist_data_quality   :: Text
    , artist_namevariations :: Maybe [Text]
    } deriving (Show, Read, Eq)

instance DiscogsResource Artist where
    type ID Artist = Int
    resourceId = artist_id
    resourceUrl = artist_resource_url

instance FromJSON Artist where
    parseJSON = withObject "artist" $ \o -> do
        artist_profile          <- o .: "profile"
        artist_releases_url     <- o .: "releases_url"
        artist_name             <- o .: "name"
        artist_realname         <- o .:? "realname"
        artist_groups           <- optional (o .: "groups")
        artist_uri              <- o .: "uri"
        artist_members          <- o .:? "members"
        artist_urls             <- o .:? "urls"
        artist_images           <- o .:? "images"
        artist_resource_url     <- o .: "resource_url"
        artist_aliases          <- optional (o .: "aliases")
        artist_id               <- o .: "id"
        artist_data_quality     <- o .: "data_quality"
        artist_namevariations   <- o .:? "namevariations"
        return Artist{..}

instance ToJSON Artist where
    toJSON Artist{..} = object [
        "profile"        .= artist_profile,
        "releases_url"   .= artist_releases_url,
        "name"           .= artist_name,
        "realname"       .= artist_realname,
        "groups"         .= artist_groups,
        "uri"            .= artist_uri,
        "members"        .= artist_members,
        "urls"           .= artist_urls,
        "images"         .= artist_images,
        "resource_url"   .= artist_resource_url,
        "aliases"        .= artist_aliases,
        "id"             .= artist_id,
        "data_quality"   .= artist_data_quality,
        "namevariations" .= artist_namevariations ]

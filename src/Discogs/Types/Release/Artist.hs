{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Release.Artist where

import Data.Text
import Data.Aeson
import Discogs.Tools

data ReleaseArtist = ReleaseArtist
     { ra_join         :: Text
     , ra_name         :: Text
     , ra_anv          :: Text
     , ra_tracks       :: Text
     , ra_role         :: Text
     , ra_resource_url :: Text
     , ra_id           :: Int
     } deriving (Show, Read, Eq)

instance DiscogsResource ReleaseArtist where
    type ID ReleaseArtist = Int
    resourceId = ra_id
    resourceUrl = ra_resource_url

instance FromJSON ReleaseArtist where
    parseJSON = withObject "artist" $ \o -> do
        ra_join <- o .: "join"
        ra_name <- o .: "name"
        ra_anv  <- o .: "anv"
        ra_tracks  <- o .: "tracks"
        ra_role  <- o .: "role"
        ra_resource_url  <- o .:  "resource_url"
        ra_id <- o .: "id"
        return ReleaseArtist{..}

instance ToJSON ReleaseArtist where
    toJSON ReleaseArtist{..} = object [
        "join" .= ra_join,
        "name" .= ra_name,
        "anv" .= ra_anv,
        "tracks" .= ra_tracks,
        "role" .= ra_role,
        "resource_url" .= ra_resource_url,
        "id" .= ra_id ]

module Discogs.Types.User.BasicInfo where

import Discogs.Types.Release.Label
import Discogs.Types.Release.Format
import Discogs.Types.Release.Artist

import Data.Text
import Data.Aeson
import Discogs.Tools

data BasicInfo = BasicInfo
    { bi_labels       :: [ReleaseLabel]
    , bi_formats      :: [Format]
    , bi_thumb        :: Text
    , bi_title        :: Text
    , bi_artists      :: [ReleaseArtist]
    , bi_resource_url :: Text
    , bi_year         :: Int
    , bi_id           :: Int
    } deriving (Show, Read, Eq)

instance DiscogsResource BasicInfo where
    type ID BasicInfo = Int
    resourceId        = bi_id
    resourceUrl       = bi_resource_url

instance FromJSON BasicInfo where
    parseJSON = withObject "basic_information" $ \o -> do
        bi_labels       <- o .: "labels"
        bi_formats      <- o .: "formats"
        bi_thumb        <- o .: "thumb"
        bi_title        <- o .: "title"
        bi_artists      <- o .: "artists"
        bi_resource_url <- o .: "resource_url"
        bi_year         <- o .: "year"
        bi_id           <- o .: "id"
        return BasicInfo{..}

instance ToJSON BasicInfo where
    toJSON BasicInfo{..} = object [
        "labels"       .= bi_labels,
        "formats"      .= bi_formats,
        "thumb"        .= bi_thumb,
        "title"        .= bi_title,
        "artists"      .= bi_artists,
        "resource_url" .= bi_resource_url,
        "id"           .= bi_id ]

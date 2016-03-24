{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.BasicInfo where

import Discogs.Types.Release.Label hiding (resource_url)
import Discogs.Types.Release.Format
import Discogs.Types.Release.Artist hiding (resource_url)

import Data.Text
import Data.Aeson

data BasicInfo = BasicInfo
    { labels       :: [Label]
    , formats      :: [Format]
    , thumb        :: Text
    , title        :: Text
    , artists      :: [Artist]
    , resource_url :: Text
    , year         :: Int
    , _id          :: Int
    } deriving (Show, Read, Eq)

instance FromJSON BasicInfo where
    parseJSON = withObject "basic_information" $ \o -> do
        labels       <- o .: "labels"
        formats      <- o .: "formats"
        thumb        <- o .: "thumb"
        title        <- o .: "title"
        artists      <- o .: "artists"
        resource_url <- o .: "resource_url"
        year         <- o .: "year"
        _id          <- o .: "id"
        return BasicInfo{..}

instance ToJSON BasicInfo where
    toJSON BasicInfo{..} = object [
        "labels"       .= labels,
        "formats"      .= formats,
        "thumb"        .= thumb,
        "title"        .= title,
        "artists"      .= artists,
        "resource_url" .= resource_url,
        "id"           .= _id ]

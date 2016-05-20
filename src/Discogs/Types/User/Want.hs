{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.Want where

import Discogs.Types.Pagination
import Discogs.Types.User.BasicInfo

import Data.Text
import Data.Aeson
import Data.Time.Clock

data Wants = Wants
    { wants_pagination :: Maybe Pagination
    , wants_wants      :: [Want]
    } deriving (Show, Read, Eq)

instance FromJSON Wants where
    parseJSON = withObject "searchResult" $ \o -> do
        wants_pagination <- o.:? "pagination"
        wants_wants   <- o .: "wants"
        return Wants{..}

instance ToJSON Wants where
    toJSON Wants{..} = object [
        "pagination" .= wants_pagination ,
        "wants"   .= wants_wants ]

data Want = Want
    { want_rating            :: Int
    , want_resource_url      :: Text
    , want_basic_information :: BasicInfo
    , want_id                :: Int
    , want_date_added        :: UTCTime
    } deriving (Show, Read, Eq)

instance FromJSON Want where
    parseJSON = withObject "want" $ \o -> do
        want_rating            <- o .: "rating"
        want_resource_url      <- o .: "resource_url"
        want_basic_information <- o .: "basic_information"
        want_id                <- o .: "id"
        want_date_added        <- o .: "date_added"
        return Want{..}

instance ToJSON Want where
    toJSON Want{..} = object [
        "rating"            .= want_rating,
        "resource_url"      .= want_resource_url,
        "basic_information" .= want_basic_information,
        "id"                .= want_id,
        "date_added"        .= want_date_added ]

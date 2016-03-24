{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Want where

import Discogs.Types.Pagination
import Discogs.Types.User.BasicInfo

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock

data Wants = Wants
    { pagination :: Maybe Pagination
    , wants      :: [Want]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Want = Want
    { rating            :: Int
    , resource_url      :: Text
    , basic_information :: BasicInfo
    , _id               :: Int
    , date_added        :: UTCTime
    } deriving (Show, Read, Eq)

instance FromJSON Want where
    parseJSON = withObject "want" $ \o -> do
        rating            <- o .: "rating"
        resource_url      <- o .: "resource_url"
        basic_information <- o .: "basic_information"
        _id               <- o .: "id"
        date_added        <- o .: "date_added"
        return Want{..}

instance ToJSON Want where
    toJSON Want{..} = object [
        "rating"            .= rating,
        "resource_url"      .= resource_url,
        "basic_information" .= basic_information,
        "id"                .= _id,
        "date_added"        .= date_added ]

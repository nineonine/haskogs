{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Release where

import Discogs.Types.Pagination
import Discogs.Types.User.BasicInfo

import Data.Aeson
import GHC.Generics
import Data.Time.Clock

data Releases  = Releases
    { pagination :: Maybe Pagination
    , releases   :: [Release]
    } deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data Release = Release
    { instance_id       :: Int
    , rating            :: Int
    , basic_information :: BasicInfo
    , folder_id         :: Int
    , date_added        :: UTCTime
    , _id               :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Release where
    parseJSON = withObject "release" $ \o -> do
        instance_id       <- o .: "instance_id"
        rating            <- o .: "rating"
        basic_information <- o .: "basic_information"
        folder_id         <- o .: "folder_id"
        date_added        <- o .: "date_added"
        _id               <- o .: "id"
        return Release{..}

instance ToJSON Release where
    toJSON Release{..} = object [
        "instance_id"       .= instance_id,
        "rating"            .= rating,
        "basic_information" .= basic_information,
        "folder_id"         .= folder_id,
        "date_added"        .= date_added,
        "id"                .= _id ]

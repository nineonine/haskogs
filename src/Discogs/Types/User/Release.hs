{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Release where

import Discogs.Types.Pagination
import Discogs.Types.User.BasicInfo

import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Time.Clock

data UserReleases  = UserReleases
    { pagination :: Maybe Pagination
    , releases   :: [UserRelease]
    } deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data UserRelease = UserRelease
    { instance_id       :: Maybe Int
    , rating            :: Int
    , notes             :: Maybe Text
    , basic_information :: BasicInfo
    , folder_id         :: Maybe Int
    , date_added        :: UTCTime
    , _id               :: Int
    } deriving (Show, Read, Eq)

instance FromJSON UserRelease where
    parseJSON = withObject "release" $ \o -> do
        instance_id       <- o .:? "instance_id"
        rating            <- o .: "rating"
        notes             <- o .:? "notes"
        basic_information <- o .: "basic_information"
        folder_id         <- o .:? "folder_id"
        date_added        <- o .: "date_added"
        _id               <- o .: "id"
        return UserRelease{..}

instance ToJSON UserRelease where
    toJSON UserRelease{..} = object [
        "instance_id"       .= instance_id,
        "rating"            .= rating,
        "basic_information" .= basic_information,
        "folder_id"         .= folder_id,
        "date_added"        .= date_added,
        "id"                .= _id ]

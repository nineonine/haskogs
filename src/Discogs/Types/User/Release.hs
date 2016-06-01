{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.Release where

import Discogs.Tools
import Discogs.Types.Pagination
import Discogs.Types.User.BasicInfo

import Data.Aeson
import Data.Text
import Data.Time.Clock

data UserReleases  = UserReleases
    { ur_pagination :: Maybe Pagination
    , ur_releases   :: [UserRelease]
    } deriving (Read, Show, Eq)

instance Paginated UserReleases where
    type Content UserReleases = [UserRelease]
    pagination = ur_pagination
    contents = ur_releases

instance FromJSON UserReleases where
    parseJSON = withObject "searchResult" $ \o -> do
        ur_pagination <- o.:? "pagination"
        ur_releases   <- o .: "releases"
        return UserReleases{..}

instance ToJSON UserReleases where
    toJSON UserReleases{..} = object [
        "pagination" .= ur_pagination ,
        "releases"   .= ur_releases ]

data UserRelease = UserRelease
    { ur_instance_id       :: Maybe Int
    , ur_rating            :: Int
    , ur_notes             :: Maybe Text
    , ur_basic_information :: BasicInfo
    , ur_folder_id         :: Maybe Int
    , ur_date_added        :: UTCTime
    , ur_id                :: Int
    } deriving (Show, Read, Eq)

instance FromJSON UserRelease where
    parseJSON = withObject "release" $ \o -> do
        ur_instance_id       <- o .:? "instance_id"
        ur_rating            <- o .: "rating"
        ur_notes             <- o .:? "notes"
        ur_basic_information <- o .: "basic_information"
        ur_folder_id         <- o .:? "folder_id"
        ur_date_added        <- o .: "date_added"
        ur_id                <- o .: "id"
        return UserRelease{..}

instance ToJSON UserRelease where
    toJSON UserRelease{..} = object [
        "instance_id"       .= ur_instance_id,
        "rating"            .= ur_rating,
        "basic_information" .= ur_basic_information,
        "folder_id"         .= ur_folder_id,
        "date_added"        .= ur_date_added,
        "id"                .= ur_id ]

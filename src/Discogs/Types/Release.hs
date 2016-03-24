{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release where

import Discogs.Types.Release.Video
import Discogs.Types.Release.Community
import Discogs.Types.Release.Artist
import Discogs.Types.Release.Track
import Discogs.Types.Release.Identifier
import Discogs.Types.Release.Company
import Discogs.Types.Release.Format
import Discogs.Types.Release.Label
import Discogs.Types.Image

import Data.Text
import Data.Aeson
import GHC.Generics

data Release = Release
    { styles             :: Maybe [Text]
    , videos             :: Maybe [Video]
    , series             :: [Label]
    , released_formatted :: Text
    , labels             :: [Label]
    , community          :: Community
    , year               :: Int
    , images             :: [Image]
    , format_quantity    :: Int
    , id                 :: Int
    , genres             :: [Text]
    , thumb              :: Text
    , extraartists       :: Maybe [Artist]
    , title              :: Text
    , artists            :: [Artist]
    , date_changed       :: Text
    , master_id          :: Maybe Int
    , tracklist          :: Maybe [Track]
    , status             :: Text
    , estimated_weight   :: Maybe Int
    , master_url         :: Maybe Text
    , released           :: Text
    , date_added         :: Text
    , country            :: Maybe Text
    , notes              :: Maybe Text
    , identifiers        :: [Identifier]
    , companies          :: [Company]
    , uri                :: Text
    , formats            :: [Format]
    , resource_url       :: Text
    , data_quality       :: Text
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

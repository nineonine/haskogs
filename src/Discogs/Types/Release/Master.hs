{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Master where

import Discogs.Types.Release.Artist
import Discogs.Types.Release.Video
import Discogs.Types.Release.Track
import Discogs.Types.Image

import Data.Text
import Data.Aeson
import GHC.Generics

data Master = Master
      { styles             :: [Text]
      , main_release       :: Int
      , main_release_url   :: Text
      , resource_url       :: Text
      , versions_url       :: Text
      , videos             :: Maybe [Video]
      , year               :: Int
      , images             :: [Image]
      , id                 :: Int
      , genres             :: [Text]
      , title              :: Text
      , artists            :: [Artist]
      , tracklist          :: [Track]
      , uri                :: Text
      , data_quality       :: Text
      } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

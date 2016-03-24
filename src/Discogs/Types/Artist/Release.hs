{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Artist.Release where

import Data.Text
import Data.Aeson
import GHC.Generics

data Release = Release
    { thumb        :: Text
    , artist       :: Text
    , main_release :: Maybe Text
    , title        :: Text
    , role         :: Text
    , year         :: Int
    , resource_url :: Text
    , release_type :: Maybe Text
    , id           :: Int
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

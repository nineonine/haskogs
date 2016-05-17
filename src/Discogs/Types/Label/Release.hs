{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Label.Release where

import Discogs.Types.Pagination

import Data.Text
import Data.Aeson
import GHC.Generics

data LabelRelease = LabelRelease
    { status       :: Text
    , thumb        :: Text
    , title        :: Text
    , format       :: Text
    , catno        :: Text
    , year         :: Maybe Int
    , resource_url :: Text
    , artist       :: Text
    , id           :: Int
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data LabelReleases = LabelReleases
    { pagination :: Maybe Pagination
    , releases   :: [LabelRelease]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

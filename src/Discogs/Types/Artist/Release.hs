{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Artist.Release where

import Discogs.Types.Pagination

import Data.Text
import Data.Aeson
import GHC.Generics

data ArtistRelease = ArtistRelease
    { thumb        :: Text
    , artist       :: Text
    , main_release :: Maybe Int
    , title        :: Text
    , role         :: Text
    , year         :: Maybe Int
    , resource_url :: Text
    , release_type :: Maybe Text
    , id           :: Int
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data ArtistReleases = ArtistReleases
    { pagination :: Maybe Pagination
    , releases   :: [ArtistRelease]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

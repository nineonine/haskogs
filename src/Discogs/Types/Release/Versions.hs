{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Versions where

import Discogs.Types.Pagination

import Data.Text
import Data.Aeson
import GHC.Generics


data ReleaseVersion = ReleaseVersion
    { status       :: Text
    , thumb        :: Text
    , format       :: Text
    , title        :: Text
    , label        :: Text
    , released     :: Text
    , catno        :: Text
    , resource_url :: Text
    , id           :: Int
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

data Versions = Versions
    { pagination :: Maybe Pagination
    , versions   :: [ReleaseVersion]
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

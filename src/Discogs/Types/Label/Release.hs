{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Label.Release where

import Data.Text
import Data.Aeson
import GHC.Generics

data Release = Release
    { status       :: Text
    , thumb        :: Text
    , title        :: Text
    , format       :: Text
    , catno        :: Text
    , year         :: Int
    , resource_url :: Text
    , artist       :: Text
    , id           :: Int
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

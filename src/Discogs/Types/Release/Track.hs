{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Track where

import Data.Text
import Data.Aeson
import GHC.Generics

data Track = Track
    { duration :: Text
    , position :: Text
    , type_    :: Text
    , title    :: Text
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

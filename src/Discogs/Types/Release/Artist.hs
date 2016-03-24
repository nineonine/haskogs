{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Artist where

import Data.Text
import Data.Aeson
import GHC.Generics

data Artist = Artist
     { join         :: Text
     , name         :: Text
     , anv          :: Text
     , tracks       :: Text
     , role         :: Text
     , resource_url :: Text
     , id           :: Int
     } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

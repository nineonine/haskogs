{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Video where

import Data.Text
import Data.Aeson
import GHC.Generics

data Video = Video
      { duration    :: Int
      , description :: Text
      , embed       :: Bool
      , uri         :: Text
      , title       :: Text
      } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

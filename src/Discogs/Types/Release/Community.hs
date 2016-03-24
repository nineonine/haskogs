{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Community where

import Discogs.Types.Release.Contributor
import Discogs.Types.Release.Rating

import Data.Text
import Data.Aeson
import GHC.Generics

data Community = Community
  { status       :: Text
  , rating       :: Rating
  , want         :: Int
  , contributors :: [Contributor]
  , have         :: Int
  , submitter    :: Contributor
  , data_quality :: Text
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

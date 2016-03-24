{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Contributor where

import Data.Text
import Data.Aeson
import GHC.Generics

data Contributor = Contributor
  { username     :: Text
  , resource_url :: Text
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

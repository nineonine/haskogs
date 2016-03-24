{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Rating where

import Data.Aeson
import GHC.Generics

data Rating = Rating
  { count   :: Int
  , average :: Double
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

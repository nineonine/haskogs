{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Contribution where

import Discogs.Types.Pagination
import Discogs.Types.Release

import GHC.Generics
import Data.Aeson hiding (Result)

data Contributions = Contributions {
      pagination    :: Maybe Pagination
    , contributions :: [Release]
} deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

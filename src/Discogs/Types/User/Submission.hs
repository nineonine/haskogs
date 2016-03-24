{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Submission where

import Discogs.Types.Pagination
import Discogs.Types.Artist
import Discogs.Types.Release
import Discogs.Types.Label

import Data.Aeson
import GHC.Generics

data Submission = Submission
    { labels   :: [Label]
    , releases :: [Release]
    , artists  :: [Artist]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Submissions = Submissions
    { pagination  :: Maybe Pagination
    , submissions :: Submission
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

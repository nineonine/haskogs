{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User.Submission where

import Discogs.Types.Pagination
import Discogs.Types.Artist
import Discogs.Types.Release
import Discogs.Types.Label

import Data.Aeson
import GHC.Generics

data Submission = Submission
    { labels   :: Maybe [Label]
    , releases :: Maybe [Release]
    , artists  :: Maybe [Artist]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Submissions = Submissions
    { subs_pagination  :: Maybe Pagination
    , subs_submissions :: Submission
    } deriving (Show, Read, Eq)

instance FromJSON Submissions where
    parseJSON = withObject "submissions" $ \o -> do
        subs_pagination <- o .:? "pagination"
        subs_submissions <- o .: "submissions"
        return Submissions{..}

instance ToJSON Submissions where
    toJSON Submissions{..} = object [
        "pagination" .= subs_pagination ,
        "submissions" .= subs_submissions ]

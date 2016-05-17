{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Label where

import Data.Text
import Data.Aeson
import GHC.Generics

data ReleaseLabel = ReleaseLabel
        { id           :: Int
        , resource_url :: Text
        , entity_type  :: Text
        , catno        :: Text
        , name         :: Text
        } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

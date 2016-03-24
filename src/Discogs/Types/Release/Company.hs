{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Company where

import Data.Text
import Data.Aeson
import GHC.Generics

data Company = Company
    { name             :: Text
    , entity_type      :: Text
    , catno            :: Text
    , resource_url     :: Text
    , id               :: Int
    , entity_type_name :: Text
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

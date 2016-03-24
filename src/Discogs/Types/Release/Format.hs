{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Release.Format where

import Data.Text
import Data.Aeson
import GHC.Generics

data Format = Format
    { qty          :: Text
    , descriptions :: Maybe [Text]
    , name         :: Text
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

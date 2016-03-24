{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Artist.Member where

import Data.Text
import Data.Aeson
import GHC.Generics

data Member = Member
    { active       :: Bool
    , resource_url :: Text
    , id           :: Int
    , name         :: Text
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

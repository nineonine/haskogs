{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Pagination where

import Data.Aeson
import GHC.Generics

data Pagination = Pagination
    { per_page :: Int
    , pages    :: Int
    , page     :: Int
    , urls     :: Object
    , items    :: Int
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

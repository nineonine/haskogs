{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.Search where

import Discogs.Types.Pagination
import Discogs.Types.Search.Result

import Data.Aeson hiding (Result)
import GHC.Generics

data Search = Search
    { pagination :: Pagination
    , results   :: [Result]
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

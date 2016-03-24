{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Discogs.Types.MarketPlace.Price where

import Data.Text
import Data.Aeson
import GHC.Generics

data Price = Price
    { currency :: Text
    , value   :: Double
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

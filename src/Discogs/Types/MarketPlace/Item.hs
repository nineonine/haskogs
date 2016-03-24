{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.MarketPlace.Item where

import Discogs.Types.MarketPlace.Price

import           Data.Aeson
import           Data.Text

data Release = Release
    { release_id  :: Int
    , description :: Text
    } deriving (Show, Read, Eq)

data Item = Item
    { release :: Release
    , price   :: Price
    , _id     :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Release where
    parseJSON = withObject "release" $ \o -> do
        release_id  <- o .: "id"
        description <- o .: "description"
        return Release{..}

instance ToJSON Release where
    toJSON Release{..} = object [
        "id"          .= release_id,
        "description" .= description ]

instance FromJSON Item where
    parseJSON = withObject "item" $ \o -> do
        _id     <- o .: "id"
        price   <- o .: "price"
        release <- o .: "release"
        return Item{..}

instance ToJSON Item where
    toJSON Item{..} = object [
        "id"      .= _id,
        "release" .= release,
        "price"   .= price ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.MarketPlace.Item where

import Discogs.Types.MarketPlace.Price

import           Data.Aeson
import           Data.Text

data ItemRelease = ItemRelease
    { release_id  :: Int
    , description :: Text
    } deriving (Show, Read, Eq)

data Item = Item
    { release :: ItemRelease
    , price   :: Price
    , _id     :: Int
    } deriving (Show, Read, Eq)

instance FromJSON ItemRelease where
    parseJSON = withObject "release" $ \o -> do
        release_id  <- o .: "id"
        description <- o .: "description"
        return ItemRelease{..}

instance ToJSON ItemRelease where
    toJSON ItemRelease{..} = object [
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

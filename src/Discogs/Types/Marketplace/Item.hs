{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Marketplace.Item where

import Discogs.Types.Marketplace.Price

import           Data.Aeson
import           Data.Text

data ItemRelease = ItemRelease
    { ir_release_id  :: Int
    , ir_description :: Text
    } deriving (Show, Read, Eq)

data Item = Item
    { item_release :: ItemRelease
    , item_price   :: Price
    , item_id     :: Int
    } deriving (Show, Read, Eq)

instance FromJSON ItemRelease where
    parseJSON = withObject "release" $ \o -> do
        ir_release_id  <- o .: "id"
        ir_description <- o .: "description"
        return ItemRelease{..}

instance ToJSON ItemRelease where
    toJSON ItemRelease{..} = object [
        "id"          .= ir_release_id,
        "description" .= ir_description ]

instance FromJSON Item where
    parseJSON = withObject "item" $ \o -> do
        item_id     <- o .: "id"
        item_price   <- o .: "price"
        item_release <- o .: "release"
        return Item{..}

instance ToJSON Item where
    toJSON Item{..} = object [
        "id"      .= item_id,
        "release" .= item_release,
        "price"   .= item_price ]

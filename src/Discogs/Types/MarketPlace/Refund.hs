{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.MarketPlace.Refund where

import           Data.Text
import           Data.Aeson
import           GHC.Generics

data Refund = Refund
    { amount :: Int
    , order  :: Maybe Order
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Order = Order
    { resource_url :: Text
    , _id          :: Text
    } deriving (Read, Show, Eq)

instance FromJSON Order where
    parseJSON = withObject "order" $ \o -> do
        resource_url <- o .: "resource_url"
        _id          <- o .: "id"
        return Order{..}

instance ToJSON Order where
    toJSON Order{..} = object [
        "resource_url" .= resource_url,
        "id"           .= _id ]

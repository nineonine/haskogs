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
    , order  :: Maybe OrderResource
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data OrderResource = OrderResource
    { resource_url :: Text
    , _id          :: Text
    } deriving (Read, Show, Eq)

instance FromJSON OrderResource where
    parseJSON = withObject "order" $ \o -> do
        resource_url <- o .: "resource_url"
        _id          <- o .: "id"
        return OrderResource{..}

instance ToJSON OrderResource where
    toJSON OrderResource{..} = object [
        "resource_url" .= resource_url,
        "id"           .= _id ]

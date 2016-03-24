{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Discogs.Types.MarketPlace.Order where

import           Discogs.Types.Alias
import           Discogs.Types.Pagination
import           Discogs.Types.MarketPlace.Item
import           Discogs.Types.MarketPlace.Price

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Data.Time.Clock

data Orders = Orders
    { pagination :: Maybe Pagination
    , orders     :: [Order]
    } deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

data Order = Order
    { _id                     :: Text
    , resource_url            :: Text
    , messages_url            :: Text
    , uri                     :: Text
    , status                  :: Text
    , next_status             :: [Text]
    , fee                     :: Price
    , created                 :: UTCTime
    , items                   :: [Item]
    , shipping                :: Price
    , shipping_address        :: Text
    , additional_instructions :: Text
    , seller                  :: Alias
    , last_activity           :: UTCTime
    , buyer                   :: Alias
    , total                   :: Price
    } deriving (Show, Read, Eq)

instance FromJSON Order where
    parseJSON = withObject "order" $ \o -> do
        _id                     <- o .: "id"
        resource_url            <- o .: "resource_url"
        messages_url            <- o .: "messages_url"
        uri                     <- o .: "uri"
        status                  <- o .: "status"
        next_status             <- o .: "next_status"
        fee                     <- o .: "fee"
        created                 <- o .: "created"
        items                   <- o .: "items"
        shipping                <- o .: "shipping"
        shipping_address        <- o .: "shipping_address"
        additional_instructions <- o .: "additional_instructions"
        seller                  <- o .: "seller"
        last_activity           <- o .: "last_activity"
        buyer                   <- o .: "buyer"
        total                   <- o .: "total"
        return Order{..}

instance ToJSON Order where
    toJSON Order{..} = object [
        "id"                      .= _id,
        "resource_url"            .= resource_url,
        "messages_url"            .= messages_url,
        "uri"                     .= uri,
        "status"                  .= status,
        "next_status"             .= next_status,
        "fee"                     .= fee,
        "created"                 .= created,
        "items"                   .= items,
        "shipping"                .= shipping,
        "shipping_address"        .= shipping_address,
        "additional_instructions" .= additional_instructions,
        "seller"                  .= seller,
        "last_activity"           .= last_activity,
        "buyer"                   .= buyer,
        "total"                   .= total ]

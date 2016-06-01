{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Marketplace.Order where

import           Discogs.Tools
import           Discogs.Types.Alias
import           Discogs.Types.Pagination
import           Discogs.Types.Marketplace.Item
import           Discogs.Types.Marketplace.Price

import           Data.Aeson
import           Data.Text
import           Data.Time.Clock

data Orders = Orders
    { orders_pagination :: Maybe Pagination
    , orders_orders     :: [Order]
    } deriving (Read, Show, Eq)

instance Paginated Orders where
    type Content Orders = [Order]
    pagination = orders_pagination
    contents = orders_orders

instance FromJSON Orders where
    parseJSON = withObject "searchResult" $ \o -> do
        orders_pagination <- o.:? "pagination"
        orders_orders   <- o .: "orders"
        return Orders{..}

instance ToJSON Orders where
    toJSON Orders{..} = object [
        "pagination" .= orders_pagination ,
        "orders"     .= orders_orders ]

data Order = Order
    { order_id                     :: Text
    , order_resource_url            :: Text
    , order_messages_url            :: Text
    , order_uri                     :: Text
    , order_status                  :: Text
    , order_next_status             :: [Text]
    , order_fee                     :: Price
    , order_created                 :: UTCTime
    , order_items                   :: [Item]
    , order_shipping                :: Price
    , order_shipping_address        :: Text
    , order_additional_instructions :: Text
    , order_seller                  :: Alias
    , order_last_activity           :: UTCTime
    , order_buyer                   :: Alias
    , order_total                   :: Price
    } deriving (Show, Read, Eq)

instance DiscogsResource Order where
    type ID Order = Text
    resourceId = order_id
    resourceUrl = order_resource_url

instance FromJSON Order where
    parseJSON = withObject "order" $ \o -> do
        order_id                     <- o .: "id"
        order_resource_url            <- o .: "resource_url"
        order_messages_url            <- o .: "messages_url"
        order_uri                     <- o .: "uri"
        order_status                  <- o .: "status"
        order_next_status             <- o .: "next_status"
        order_fee                     <- o .: "fee"
        order_created                 <- o .: "created"
        order_items                   <- o .: "items"
        order_shipping                <- o .: "shipping"
        order_shipping_address        <- o .: "shipping_address"
        order_additional_instructions <- o .: "additional_instructions"
        order_seller                  <- o .: "seller"
        order_last_activity           <- o .: "last_activity"
        order_buyer                   <- o .: "buyer"
        order_total                   <- o .: "total"
        return Order{..}

instance ToJSON Order where
    toJSON Order{..} = object [
        "id"                      .= order_id,
        "resource_url"            .= order_resource_url,
        "messages_url"            .= order_messages_url,
        "uri"                     .= order_uri,
        "status"                  .= order_status,
        "next_status"             .= order_next_status,
        "fee"                     .= order_fee,
        "created"                 .= order_created,
        "items"                   .= order_items,
        "shipping"                .= order_shipping,
        "shipping_address"        .= order_shipping_address,
        "additional_instructions" .= order_additional_instructions,
        "seller"                  .= order_seller,
        "last_activity"           .= order_last_activity,
        "buyer"                   .= order_buyer,
        "total"                   .= order_total ]

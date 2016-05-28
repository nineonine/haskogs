{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Marketplace.Refund where

import           Data.Text
import           Data.Aeson

data Refund = Refund
    { refund_amount :: Int
    , refund_order  :: Maybe OrderResource
    } deriving (Show, Read, Eq)

instance FromJSON Refund where
    parseJSON = withObject "refund" $ \o -> do
        refund_amount <- o .: "amount"
        refund_order <- o .:? "order"
        return Refund{..}

instance ToJSON Refund where
    toJSON Refund{..} = object [
        "amount" .= refund_amount,
        "order"  .= refund_order ]

data OrderResource = OrderResource
    { or_resource_url :: Text
    , or_id          :: Text
    } deriving (Read, Show, Eq)

instance FromJSON OrderResource where
    parseJSON = withObject "order" $ \o -> do
        or_resource_url <- o .: "resource_url"
        or_id          <- o .: "id"
        return OrderResource{..}

instance ToJSON OrderResource where
    toJSON OrderResource{..} = object [
        "resource_url" .= or_resource_url,
        "id"           .= or_id ]

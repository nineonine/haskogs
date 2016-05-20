{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.MarketPlace.Price where

import Data.Text
import Data.Aeson

data Price = Price
    { price_currency :: Text
    , price_value    :: Double
    } deriving (Show, Read, Eq)

instance FromJSON Price where
    parseJSON = withObject "price" $ \o -> do
        price_currency <- o .: "currency"
        price_value    <- o .: "value"
        return Price{..}

instance ToJSON Price where
    toJSON Price{..} = object [
        "currency" .= price_currency,
        "value"    .= price_value ]

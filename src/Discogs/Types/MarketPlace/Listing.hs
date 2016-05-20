{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.MarketPlace.Listing where

import Discogs.Types.Alias
import Discogs.Types.Pagination
import Discogs.Types.MarketPlace.Price
import Discogs.Types.MarketPlace.Release

import Data.Text
import Data.Aeson
import Data.Time.Clock

data Inventory = Inventory
    { inv_pagination :: Maybe Pagination
    , inv_listings   :: [Listing]
    } deriving (Show, Read, Eq)

instance FromJSON Inventory where
    parseJSON = withObject "searchResult" $ \o -> do
        inv_pagination <- o .:? "pagination"
        inv_listings   <- o .: "listings"
        return Inventory{..}

instance ToJSON Inventory where
    toJSON Inventory{..} = object [
        "pagination" .= inv_pagination,
        "listings"   .= inv_listings ]

data Listing = Listing
    { listing_weight           :: Maybe Int
    , listing_format_quantity  :: Maybe Int
    , listing_external_id      :: Maybe Text
    , listing_location         :: Maybe Text
    , listing_status           :: Text
    , listing_price            :: Price
    , listing_allow_offers     :: Bool
    , listing_sleeve_condition :: Text
    , listing_id               :: Int
    , listing_condition        :: Text
    , listing_posted           :: UTCTime
    , listing_ships_from       :: Text
    , listing_uri              :: Text
    , listing_comments         :: Text
    , listing_seller           :: Alias
    , listing_release          :: ListingRelease
    , listing_resource_url     :: Text
    , listing_audio            :: Bool
    } deriving (Read, Show, Eq)

instance FromJSON Listing where
    parseJSON = withObject "listing" $ \o -> do
        listing_weight           <- o .:? "weight"
        listing_format_quantity  <- o .:? "format_quantity"
        listing_external_id      <- o .:? "external_id"
        listing_location         <- o .:? "location"
        listing_sleeve_condition <- o .: "sleeve_condition"
        listing_status           <- o .: "status"
        listing_price            <- o .: "price"
        listing_allow_offers     <- o .: "allow_offers"
        listing_id               <- o .: "id"
        listing_condition        <- o .: "condition"
        listing_posted           <- o .: "posted"
        listing_ships_from       <- o .: "ships_from"
        listing_uri              <- o .: "uri"
        listing_comments         <- o .: "comments"
        listing_seller           <- o .: "seller"
        listing_release          <- o .: "release"
        listing_resource_url     <- o .: "resource_url"
        listing_audio            <- o .: "audio"
        return Listing{..}


instance ToJSON Listing where
    toJSON Listing{..} = object [
        "weight"           .= listing_weight,
        "format_quantity"  .= listing_format_quantity,
        "external_id"      .= listing_external_id,
        "location"         .= listing_location,
        "sleeve_condition" .= listing_sleeve_condition,
        "status"           .= listing_status,
        "price"            .= listing_price,
        "allow_offers"     .= listing_allow_offers,
        "id"               .= listing_id,
        "condition"        .= listing_condition,
        "posted"           .= listing_posted,
        "ships_from"       .= listing_ships_from,
        "uri"              .= listing_uri,
        "comments"         .= listing_comments,
        "seller"           .= listing_seller,
        "release"          .= listing_release,
        "resource_url"     .= listing_resource_url,
        "audio"            .= listing_audio]

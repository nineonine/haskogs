{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.MarketPlace.Listing where

import Discogs.Types.Alias
import Discogs.Types.Pagination
import Discogs.Types.MarketPlace.Price
import Discogs.Types.MarketPlace.Release

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock

data Inventory = Inventory
    { pagination :: Maybe Pagination
    , listings   :: [Listing]
    } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Listing = Listing
    { weight           :: Maybe Int
    , format_quantity  :: Maybe Int
    , external_id      :: Maybe Int
    , location         :: Maybe Text
    , status           :: Text
    , price            :: Price
    , allow_offers     :: Bool
    , sleeve_condition :: Text
    , _id              :: Int
    , condition        :: Text
    , posted           :: UTCTime
    , ships_from       :: Text
    , uri              :: Text
    , comments         :: Text
    , seller           :: Alias
    , release          :: Release
    , resource_url     :: Text
    , audio            :: Bool
    } deriving (Read, Show, Eq)

instance FromJSON Listing where
    parseJSON = withObject "listing" $ \o -> do
        weight           <- o .:? "weight"
        format_quantity  <- o .:? "format_quantity"
        external_id      <- o .:? "external_id"
        location         <- o .:? "location"
        sleeve_condition <- o .: "sleeve_condition"
        status           <- o .: "status"
        price            <- o .: "price"
        allow_offers     <- o .: "allow_offers"
        _id              <- o .: "id"
        condition        <- o .: "condition"
        posted           <- o .: "posted"
        ships_from       <- o .: "ships_from"
        uri              <- o .: "uri"
        comments         <- o .: "comments"
        seller           <- o .: "seller"
        release          <- o .: "release"
        resource_url     <- o .: "resource_url"
        audio            <- o .: "audio"
        return Listing{..}


instance ToJSON Listing where
    toJSON Listing{..} = object [
        "weight"           .= weight,
        "format_quantity"  .= format_quantity,
        "external_id"      .= external_id,
        "location"         .= location,
        "sleeve_condition" .= sleeve_condition,
        "status"           .= status,
        "price"            .= price,
        "allow_offers"     .= allow_offers,
        "id"               .= _id,
        "condition"        .= condition,
        "posted"           .= posted,
        "ships_from"       .= ships_from,
        "uri"              .= uri,
        "comments"         .= comments,
        "seller"           .= seller,
        "release"          .= release,
        "resource_url"     .= resource_url,
        "audio"            .= audio]

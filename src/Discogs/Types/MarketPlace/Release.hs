{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.MarketPlace.Release where

import Data.Text
import Data.Aeson

data Release = Release
    { catalog_number :: Text
    , resource_url   :: Text
    , year           :: Int
    , _id            :: Int
    , description    :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Release where
    parseJSON = withObject "release" $ \o -> do
        catalog_number <- o .: "catalog_number"
        resource_url   <- o .: "resource_url"
        year           <- o .: "year"
        _id            <- o .: "id"
        description    <- o .: "description"
        return Release{..}

instance ToJSON Release where
    toJSON Release{..} = object [
        "catalog_number" .= catalog_number,
        "resource_url"   .= resource_url,
        "year"           .= year,
        "id"             .= _id,
        "description"    .= description]

module Discogs.Types.Marketplace.Release where

import Data.Text
import Data.Aeson
import Discogs.Tools

data ListingRelease = ListingRelease
    { listr_catalog_number :: Text
    , listr_resource_url   :: Text
    , listr_year           :: Int
    , listr_id             :: Int
    , listr_description    :: Text
    } deriving (Show, Read, Eq)

instance DiscogsResource ListingRelease where
    type ID ListingRelease = Int
    resourceId             = listr_id
    resourceUrl            = listr_resource_url

instance FromJSON ListingRelease where
    parseJSON = withObject "release" $ \o -> do
        listr_catalog_number <- o .: "catalog_number"
        listr_resource_url   <- o .: "resource_url"
        listr_year           <- o .: "year"
        listr_id             <- o .: "id"
        listr_description    <- o .: "description"
        return ListingRelease{..}

instance ToJSON ListingRelease where
    toJSON ListingRelease{..} = object [
        "catalog_number" .= listr_catalog_number,
        "resource_url"   .= listr_resource_url,
        "year"           .= listr_year,
        "id"             .= listr_id,
        "description"    .= listr_description]

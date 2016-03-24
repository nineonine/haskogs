{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.User where

import Data.Text
import Data.Time.Clock
import Data.Aeson hiding (Result)

data User = User
    { profile               :: Text
    , email                 :: Maybe Text
    , wantlist_url          :: Text
    , rank                  :: Int
    , num_pending           :: Int
    , uid                   :: Int
    , num_for_sale          :: Int
    , home_page             :: Text
    , location              :: Text
    , collection_fields_url :: Text
    , releases_contributed  :: Int
    , registered            :: UTCTime
    , rating_avg            :: Double
    , num_collection        :: Int
    , releases_rated        :: Int
    , num_lists             :: Int
    , name                  :: Text
    , num_wantlist          :: Int
    , inventory_url         :: Text
    , uri                   :: Text
    , avatar_url            :: Text
    , resource_url          :: Text
    } deriving (Show, Read, Eq)

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        email                 <- o .: "email"
        profile               <- o .: "profile"
        wantlist_url          <- o .: "wantlist_url"
        rank                  <- o .: "rank"
        num_pending           <- o .: "num_pending"
        uid                   <- o .: "id"
        num_for_sale          <- o .: "num_for_sale"
        home_page             <- o .: "home_page"
        location              <- o .: "location"
        collection_fields_url <- o .: "collection_fields_url"
        releases_contributed  <- o .: "releases_contributed"
        registered            <- o .: "registered"
        rating_avg            <- o .: "rating_avg"
        num_collection        <- o .: "num_collection"
        releases_rated        <- o .: "releases_rated"
        num_lists             <- o .: "num_lists"
        name                  <- o .: "name"
        num_wantlist          <- o .: "num_wantlist"
        inventory_url         <- o .: "inventory_url"
        uri                   <- o .: "uri"
        avatar_url            <- o .: "avatar_url"
        resource_url          <- o .: "resource_url"
        return User{..}

instance ToJSON User where
    toJSON User{..} = object [
        "email"                 .= email,
        "profile"               .= profile,
        "wantlist_url"          .= wantlist_url,
        "rank"                  .= rank,
        "num_pending"           .= num_pending,
        "id"                    .= uid,
        "num_for_sale"          .= num_for_sale,
        "home_page"             .= home_page,
        "location"              .= location,
        "collection_fields_url" .= collection_fields_url,
        "releases_contributed"  .= releases_contributed,
        "registered"            .= registered,
        "rating_avg"            .= rating_avg,
        "num_collection"        .= num_collection,
        "releases_rated"        .= releases_rated,
        "num_lists"             .= num_lists,
        "name"                  .= name,
        "num_wantlist"          .= num_wantlist,
        "inventory_url"         .= inventory_url,
        "uri"                   .= uri,
        "avatar_url"            .= avatar_url,
        "resource_url"          .= resource_url ]

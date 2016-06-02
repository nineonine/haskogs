module Discogs.Types.User where

import Discogs.Tools
import Data.Text
import Data.Time.Clock
import Data.Aeson hiding (Result)

data User = User
    { user_profile               :: Text
    , user_email                 :: Maybe Text
    , user_wantlist_url          :: Text
    , user_rank                  :: Int
    , user_num_pending           :: Int
    , user_id                    :: Int
    , user_num_for_sale          :: Int
    , user_home_page             :: Text
    , user_location              :: Text
    , user_collection_fields_url :: Text
    , user_releases_contributed  :: Int
    , user_registered            :: UTCTime
    , user_rating_avg            :: Double
    , user_num_collection        :: Int
    , user_releases_rated        :: Int
    , user_num_lists             :: Int
    , user_name                  :: Text
    , user_num_wantlist          :: Int
    , user_inventory_url         :: Text
    , user_uri                   :: Text
    , user_avatar_url            :: Text
    , user_resource_url     :: Text
    } deriving (Show, Read, Eq)

instance DiscogsResource User where
    type ID User = Int
    resourceId = user_id
    resourceUrl = user_resource_url

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        user_email                 <- o .: "email"
        user_profile               <- o .: "profile"
        user_wantlist_url          <- o .: "wantlist_url"
        user_rank                  <- o .: "rank"
        user_num_pending           <- o .: "num_pending"
        user_id                    <- o .: "id"
        user_num_for_sale          <- o .: "num_for_sale"
        user_home_page             <- o .: "home_page"
        user_location              <- o .: "location"
        user_collection_fields_url <- o .: "collection_fields_url"
        user_releases_contributed  <- o .: "releases_contributed"
        user_registered            <- o .: "registered"
        user_rating_avg            <- o .: "rating_avg"
        user_num_collection        <- o .: "num_collection"
        user_releases_rated        <- o .: "releases_rated"
        user_num_lists             <- o .: "num_lists"
        user_name                  <- o .: "name"
        user_num_wantlist          <- o .: "num_wantlist"
        user_inventory_url         <- o .: "inventory_url"
        user_uri                   <- o .: "uri"
        user_avatar_url            <- o .: "avatar_url"
        user_resource_url          <- o .: "resource_url"
        return User{..}

instance ToJSON User where
    toJSON User{..} = object [
        "email"                 .= user_email,
        "profile"               .= user_profile,
        "wantlist_url"          .= user_wantlist_url,
        "rank"                  .= user_rank,
        "num_pending"           .= user_num_pending,
        "id"                    .= user_id,
        "num_for_sale"          .= user_num_for_sale,
        "home_page"             .= user_home_page,
        "location"              .= user_location,
        "collection_fields_url" .= user_collection_fields_url,
        "releases_contributed"  .= user_releases_contributed,
        "registered"            .= user_registered,
        "rating_avg"            .= user_rating_avg,
        "num_collection"        .= user_num_collection,
        "releases_rated"        .= user_releases_rated,
        "num_lists"             .= user_num_lists,
        "name"                  .= user_name,
        "num_wantlist"          .= user_num_wantlist,
        "inventory_url"         .= user_inventory_url,
        "uri"                   .= user_uri,
        "avatar_url"            .= user_avatar_url,
        "resource_url"          .= user_resource_url ]

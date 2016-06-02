module Discogs.Types.User.Identity where

import Data.Text
import Data.Aeson
import Discogs.Tools

data Identity = Identity
    { id_resource_url  :: Text
    , id_id            :: Int
    , id_username      :: Text
    , id_consumer_name :: Text
    } deriving (Show, Read, Eq)

instance DiscogsResource Identity where
    type ID Identity = Int
    resourceId       = id_id
    resourceUrl      = id_resource_url

instance FromJSON Identity where
    parseJSON = withObject "identity" $ \o -> do
        id_resource_url  <- o .: "resource_url"
        id_id            <- o .: "id"
        id_username      <- o .: "username"
        id_consumer_name <- o .: "consumer_name"
        return Identity{..}

instance ToJSON Identity where
    toJSON Identity{..} = object [
        "resource_url"  .= id_resource_url,
        "id"            .= id_id,
        "username"      .= id_username,
        "consumer_name" .= id_consumer_name ]

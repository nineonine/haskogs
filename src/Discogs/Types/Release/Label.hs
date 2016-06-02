module Discogs.Types.Release.Label where

import Data.Text
import Data.Aeson
import Discogs.Tools

data ReleaseLabel = ReleaseLabel
        { rl_id           :: Int
        , rl_resource_url :: Text
        , rl_entity_type  :: Text
        , rl_catno        :: Text
        , rl_name         :: Text
        } deriving (Show, Read, Eq)

instance DiscogsResource ReleaseLabel where
    type ID ReleaseLabel = Int
    resourceId           = rl_id
    resourceUrl          = rl_resource_url

instance FromJSON ReleaseLabel where
    parseJSON = withObject "label" $ \o -> do
        rl_id           <- o .: "id"
        rl_resource_url <- o .: "resource_url"
        rl_entity_type  <- o .: "entity_type"
        rl_catno        <- o .: "catno"
        rl_name         <- o .: "name"
        return ReleaseLabel{..}

instance ToJSON ReleaseLabel where
    toJSON ReleaseLabel{..} = object [
        "id"           .= rl_id,
        "resource_url" .= rl_resource_url,
        "entity_type"  .= rl_entity_type,
        "catno"        .= rl_catno,
        "name"         .= rl_name ]

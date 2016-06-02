module Discogs.Types.Image where

import           Data.Aeson
import           Data.Text

data Image = Image
    { img_uri          :: Text
    , img_height       :: Int
    , img_width        :: Int
    , img_resource_url :: Text
    , img_type         :: Text
    , img_uri150       :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Image where
    parseJSON = withObject "image" $ \o -> do
        img_uri          <- o .: "uri"
        img_height       <- o .: "height"
        img_width        <- o .: "width"
        img_resource_url <- o .: "resource_url"
        img_type         <- o .: "type"
        img_uri150       <- o .: "uri150"
        return Image{..}

instance ToJSON Image where
    toJSON Image{..} = object [
        "uri"          .= img_uri,
        "height"       .= img_height,
        "width"        .= img_width,
        "resource_url" .= img_resource_url,
        "type"         .= img_type,
        "uri150"       .= img_uri150 ]

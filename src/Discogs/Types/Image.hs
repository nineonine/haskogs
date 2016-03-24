{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Discogs.Types.Image where

import Data.Text
import Data.Aeson

data Image = Image
    { uri          :: Text
    , height       :: Int
    , width        :: Int
    , resource_url :: Text
    , imgtype      :: Text
    , uri150       :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Image where
    parseJSON = withObject "image" $ \o -> do
        uri          <- o .: "uri"
        height       <- o .: "height"
        width        <- o .: "width"
        resource_url <- o .: "resource_url"
        imgtype      <- o .: "type"
        uri150       <- o .: "uri150"
        return Image{..}

instance ToJSON Image where
    toJSON Image{..} = object [
        "uri"          .= uri,
        "height"       .= height,
        "width"        .= width,
        "resource_url" .= resource_url,
        "type"         .= imgtype,
        "uri150"       .= uri150 ]

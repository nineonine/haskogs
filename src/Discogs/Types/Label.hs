{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Label where

import qualified Discogs.Types.Alias as A
import qualified Discogs.Types.Image as I

import Data.Text
import Control.Applicative
import Data.Aeson

data Label = Label
    { profile      :: Maybe Text
    , releases_url :: Text
    , name         :: Text
    , contact_info :: Maybe Text
    , uri          :: Text
    , parent_label :: Maybe A.Alias
    , sublabels    :: Maybe [A.Alias]
    , urls         :: Maybe [Text]
    , images       :: Maybe [I.Image]
    , resource_url :: Text
    , _id           :: Int
    , data_quality :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Label where
    parseJSON = withObject "label" $ \o -> do
        profile      <- o .:? "profile"
        releases_url <- o .: "releases_url"
        name         <- o .: "name"
        contact_info <- o .:? "contact_info"
        uri          <- o .: "uri"
        parent_label <- o .:? "parent_label"
        sublabels    <- optional (o .: "sublabels")
        urls         <- o .:? "urls"
        images       <- o .:? "images"
        resource_url <- o .: "resource_url"
        _id          <- o .: "id"
        data_quality <- o .: "data_quality"
        return Label{..}


instance ToJSON Label where
    toJSON Label{..} = object [
        "profile"      .= profile,
        "releases_url" .= releases_url,
        "name"         .= name,
        "contact_info" .= contact_info,
        "uri"          .= uri,
        "parent_label" .= parent_label,
        "sublabels"    .= sublabels,
        "urls"         .= urls,
        "images"       .= images,
        "resource_url" .= resource_url,
        "id"           .= _id,
        "data_quality" .= data_quality ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Label where

import qualified Discogs.Types.Alias as A
import qualified Discogs.Types.Image as I

import           Control.Applicative
import           Data.Aeson
import           Data.Text

data Label = Label
    { label_profile      :: Maybe Text
    , label_releases_url :: Text
    , label_name         :: Text
    , label_contact_info :: Maybe Text
    , label_uri          :: Text
    , label_parent_label :: Maybe A.Alias
    , label_sublabels    :: Maybe [A.Alias]
    , label_urls         :: Maybe [Text]
    , label_images       :: Maybe [I.Image]
    , label_id           :: Int
    , label_data_quality :: Text
    , label_resource_url :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Label where
    parseJSON = withObject "label" $ \o -> do
        label_profile      <- o .:? "profile"
        label_releases_url <- o .: "releases_url"
        label_name   <- o .: "name"
        label_contact_info <- o .:? "contact_info"
        label_uri    <- o .: "uri"
        label_parent_label <- o .:? "parent_label"
        label_sublabels    <- optional (o .: "sublabels")
        label_urls         <- o .:? "urls"
        label_images       <- o .:? "images"
        label_resource_url <- o .: "resource_url"
        label_id     <- o .: "id"
        label_data_quality <- o .: "data_quality"
        return Label{..}


instance ToJSON Label where
    toJSON Label{..} = object [
        "profile"      .= label_profile,
        "releases_url" .= label_releases_url,
        "name"         .= label_name,
        "contact_info" .= label_contact_info,
        "uri"          .= label_uri,
        "parent_label" .= label_parent_label,
        "sublabels"    .= label_sublabels,
        "urls"         .= label_urls,
        "images"       .= label_images,
        "resource_url" .= label_resource_url,
        "id"           .= label_id,
        "data_quality" .= label_data_quality ]

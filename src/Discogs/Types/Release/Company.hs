{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Release.Company where

import           Data.Aeson
import           Data.Text

data Company = Company
    { company_name             :: Text
    , company_entity_type      :: Text
    , company_catno            :: Text
    , company_resource_url     :: Text
    , company_id               :: Int
    , company_entity_type_name :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Company where
    parseJSON = withObject "company" $ \o -> do
        company_name             <- o .: "name"
        company_entity_type      <- o .: "entity_type"
        company_catno            <- o .: "catno"
        company_resource_url     <- o .: "resource_url"
        company_id               <- o .: "id"
        company_entity_type_name <- o .: "entity_type_name"
        return Company{..}

instance ToJSON Company where
    toJSON Company{..} = object [
        "name"             .= company_name,
        "entity_type"      .= company_entity_type,
        "catno"            .= company_catno,
        "resource_url"     .= company_resource_url,
        "id"               .= company_id,
        "entity_type_name" .= company_entity_type_name ]

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Alias where

import Data.Text
import Control.Applicative
import Data.Aeson

data Alias = Alias
    { resource_url :: Text
    , _id          :: Maybe Int
    , name         :: Maybe Text
    } deriving (Show, Read, Eq)

instance FromJSON Alias where
    parseJSON = withObject "alias" $ \o -> do
        resource_url <- o .: "resource_url"
        _id          <- o .:? "id"
        name         <- optional (o .: "name" <|> o .: "username")
        return Alias{..}

instance ToJSON Alias where
    toJSON Alias{..} = object [
        "resource_url" .= resource_url,
        "id"           .= _id,
        "name"         .= name]

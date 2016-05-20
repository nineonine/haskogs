{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Release.Format where

import Data.Text
import Data.Aeson

data Format = Format
    { format_qty          :: Text
    , format_descriptions :: Maybe [Text]
    , format_name         :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Format where
    parseJSON = withObject "format" $ \o -> do
        format_qty <- o .: "qty"
        format_descriptions <- o .:? "descriptions"
        format_name <- o .: "name"
        return Format{..}

instance ToJSON Format where
    toJSON Format{..} = object [
        "qty" .= format_qty ,
        "descriptions" .= format_descriptions ,
        "name" .= format_name ]

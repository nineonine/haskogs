{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Discogs.Types.Release.Identifier where

import Data.Text
import Data.Aeson

data Identifier = Identifier
        { itype       :: Text
        , description :: Text
        , value       :: Text
        } deriving (Show, Read, Eq)

instance FromJSON Identifier where
    parseJSON = withObject "identifier" $ \o -> do
        itype       <- o .: "type"
        description <- o .: "description"
        value       <- o .: "value"
        return Identifier{..}

instance ToJSON Identifier where
    toJSON Identifier{..} = object [
        "type"        .= itype,
        "description" .= description,
        "value"       .= value ]

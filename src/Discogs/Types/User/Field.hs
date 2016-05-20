{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Field where

import Data.Text hiding (count)
import Data.Aeson
import GHC.Generics

data Fields = Fields { fields :: [Field] } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Field = Field
    { field_lines   :: Maybe Int
    , field_options  :: Maybe [Text]
    , field_position :: Int
    , field_name     :: Text
    , field_type    :: Text
    , field_id      :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Field where
    parseJSON = withObject "field" $ \o -> do
        field_lines   <- o .:? "lines"
        field_options  <- o .:? "options"
        field_position <- o .: "position"
        field_name     <- o .: "name"
        field_type    <- o .: "type"
        field_id      <- o .: "id"
        return Field{..}

instance ToJSON Field where
    toJSON Field{..} = object [
        "lines"    .= field_lines,
        "options"  .= field_options,
        "position" .= field_position,
        "name"     .= field_name,
        "type"     .= field_type,
        "id"       .= field_id ]

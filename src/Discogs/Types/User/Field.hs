{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}

module Discogs.Types.User.Field where

import Data.Text hiding (count)
import Data.Aeson
import GHC.Generics

data Fields = Fields { fields :: [Field] } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data Field = Field
    { _lines   :: Maybe Int
    , options  :: Maybe [Text]
    , position :: Int
    , name     :: Text
    , _type    :: Text
    , _id      :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Field where
    parseJSON = withObject "field" $ \o -> do
        _lines   <- o .:? "lines"
        options  <- o .:? "options"
        position <- o .: "position"
        name     <- o .: "name"
        _type    <- o .: "type"
        _id      <- o .: "id"
        return Field{..}

instance ToJSON Field where
    toJSON Field{..} = object [
        "lines"    .= _lines,
        "options"  .= options,
        "position" .= position,
        "name"     .= name,
        "type"     .= _type,
        "id"       .= _id ]

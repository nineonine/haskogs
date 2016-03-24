{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Search.Result where

import Data.Text
import Control.Applicative
import Data.Aeson hiding (Result)

data Result = Result
    { thumb        :: Text
    , style        :: Maybe [Text]
    , format       :: Maybe [Text]
    , country      :: Maybe Text
    , barcode      :: Maybe [Text]
    , community    :: Maybe Object
    , label        :: Maybe [Text]
    , catno        :: Maybe Text
    , year         :: Maybe Text
    , genre        :: Maybe [Text]
    , resource_url :: Text
    , title        :: Text
    , uri          :: Text
    , stype        :: Text
    , _id          :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Result where
    parseJSON = withObject "result" $ \o -> do
        label        <- optional (o .: "label")
        thumb        <- o .: "thumb"
        style        <- o .:? "style"
        format       <- o .:? "format"
        country      <- o .:? "country"
        barcode      <- o .:? "barcode"
        community    <- o .:? "community"
        catno        <- o .:? "catno"
        year         <- o .:? "year"
        genre        <- o .:? "genre"
        resource_url <- o .: "resource_url"
        title        <- o .: "title"
        uri          <- o .: "uri"
        stype        <- o .: "type"
        _id          <- o .: "id"
        return Result{..}

instance ToJSON Result where
    toJSON Result{..} = object [
        "thumb"        .= thumb,
        "style"        .= style,
        "format"       .= format,
        "country"      .= country,
        "barcode"      .= barcode,
        "community"    .= country,
        "barcode"      .= barcode,
        "community"    .= community,
        "label"        .= label,
        "catno"        .= catno,
        "year"         .= year,
        "genre"        .= genre,
        "resource_url" .= resource_url,
        "title"        .= title,
        "uri"          .= uri,
        "type"        .= stype,
        "id"           .= _id ]

module Discogs.Types.Search.Result where

import           Control.Applicative
import           Data.Aeson          hiding (Result)
import           Data.Text
import           Discogs.Tools

data Result = Result
    { result_thumb        :: Text
    , result_style        :: Maybe [Text]
    , result_format       :: Maybe [Text]
    , result_country      :: Maybe Text
    , result_barcode      :: Maybe [Text]
    , result_community    :: Maybe Object
    , result_label        :: Maybe [Text]
    , result_catno        :: Maybe Text
    , result_year         :: Maybe Text
    , result_genre        :: Maybe [Text]
    , result_resource_url :: Text
    , result_title        :: Text
    , result_uri          :: Text
    , result_type         :: Text
    , result_id           :: Int
    } deriving (Show, Read, Eq)

instance DiscogsResource Result where
    type ID Result = Int
    resourceId     = result_id
    resourceUrl    = result_resource_url

instance FromJSON Result where
    parseJSON = withObject "result" $ \o -> do
        result_label        <- optional (o .: "label")
        result_thumb        <- o .: "thumb"
        result_style        <- o .:? "style"
        result_format       <- o .:? "format"
        result_country      <- o .:? "country"
        result_barcode      <- o .:? "barcode"
        result_community    <- o .:? "community"
        result_catno        <- o .:? "catno"
        result_year         <- o .:? "year"
        result_genre        <- o .:? "genre"
        result_resource_url <- o .: "resource_url"
        result_title        <- o .: "title"
        result_uri          <- o .: "uri"
        result_type         <- o .: "type"
        result_id           <- o .: "id"
        return Result{..}

instance ToJSON Result where
    toJSON Result{..} = object [
        "thumb"        .= result_thumb,
        "style"        .= result_style,
        "format"       .= result_format,
        "country"      .= result_country,
        "barcode"      .= result_barcode,
        "community"    .= result_country,
        "barcode"      .= result_barcode,
        "community"    .= result_community,
        "label"        .= result_label,
        "catno"        .= result_catno,
        "year"         .= result_year,
        "genre"        .= result_genre,
        "resource_url" .= result_resource_url,
        "title"        .= result_title,
        "uri"          .= result_uri,
        "type"         .= result_type,
        "id"           .= result_id ]

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Artist where

import Discogs.Types.Artist.Member
import Discogs.Types.Image
import Discogs.Types.Alias

import Data.Text
import Data.Aeson
import Control.Applicative

data Artist = Artist
    { profile        :: Text
    , releases_url   :: Text
    , name           :: Text
    , realname       :: Maybe Text
    , groups         :: Maybe [Member]
    , uri            :: Text
    , members        :: Maybe [Member]
    , urls           :: Maybe [Text]
    , images         :: Maybe [Image]
    , resource_url   :: Text
    , aliases        :: Maybe [Alias]
    , _id            :: Int
    , data_quality   :: Text
    , namevariations :: Maybe [Text]
    } deriving (Show, Read, Eq)

instance FromJSON Artist where
    parseJSON = withObject "artist" $ \o -> do
        profile          <- o .: "profile"
        releases_url     <- o .: "releases_url"
        name             <- o .: "name"
        realname         <- o .:? "realname"
        groups           <- optional (o .: "groups")
        uri              <- o .: "uri"
        members          <- o .:? "members"
        urls             <- o .:? "urls"
        images           <- o .:? "images"
        resource_url     <- o .: "resource_url"
        aliases          <- optional (o .: "aliases")
        _id              <- o .: "id"
        data_quality     <- o .: "data_quality"
        namevariations   <- o .:? "namevariations"
        return Artist{..}

instance ToJSON Artist where
    toJSON Artist{..} = object [
        "profile"        .= profile,
        "releases_url"   .= releases_url,
        "name"           .= name,
        "realname"       .= realname,
        "groups"         .= groups,
        "uri"            .= uri,
        "members"        .= members,
        "urls"           .= urls,
        "images"         .= images,
        "resource_url"   .= resource_url,
        "aliases"        .= aliases,
        "id"             .= _id,
        "data_quality"   .= data_quality,
        "namevariations" .= namevariations ]

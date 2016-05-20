{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Pagination where

import Data.Aeson

data Pagination = Pagination
    { per_page :: Int
    , pages    :: Int
    , page     :: Int
    , pag_urls :: Object
    , items    :: Int
    } deriving (Show, Read, Eq)

instance FromJSON Pagination where
    parseJSON = withObject "image" $ \o -> do
        per_page <- o .: "per_page"
        pages    <- o .: "pages"
        page     <- o .: "page"
        pag_urls <- o .: "urls"
        items    <- o .: "items"
        return Pagination{..}

instance ToJSON Pagination where
    toJSON Pagination{..} = object [
        "per_page" .= per_page,
        "pages"    .= pages,
        "page"     .= page,
        "urls"     .= pag_urls,
        "items"    .= items ]

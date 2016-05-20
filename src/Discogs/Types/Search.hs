{-# LANGUAGE RecordWildCards, OverloadedStrings#-}

module Discogs.Types.Search where

import Discogs.Types.Pagination
import Discogs.Types.Search.Result

import Data.Aeson hiding (Result)

data SearchResult = SearchResult
    { sr_pagination :: Pagination
    , sr_results   :: [Result]
    } deriving (Show, Read, Eq)

instance FromJSON SearchResult where
    parseJSON = withObject "searchResult" $ \o -> do
        sr_pagination <- o.: "pagination"
        sr_results    <- o .: "results"
        return SearchResult{..}

instance ToJSON SearchResult where
    toJSON SearchResult{..} = object [
        "pagination" .= sr_pagination,
        "results"    .= sr_results ]

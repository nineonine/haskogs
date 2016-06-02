module Discogs.Types.Search where

import Discogs.Tools
import Discogs.Types.Pagination
import Discogs.Types.Search.Result

import Data.Aeson hiding (Result)

data SearchResult = SearchResult
    { sr_pagination :: Pagination
    , sr_results    :: [Result]
    } deriving (Show, Read, Eq)

instance Paginated SearchResult where
    type Content SearchResult = [Result]
    pagination                = Just . sr_pagination
    contents                  = sr_results

instance FromJSON SearchResult where
    parseJSON = withObject "searchResult" $ \o -> do
        sr_pagination <- o.: "pagination"
        sr_results    <- o .: "results"
        return SearchResult{..}

instance ToJSON SearchResult where
    toJSON SearchResult{..} = object [
        "pagination" .= sr_pagination,
        "results"    .= sr_results ]

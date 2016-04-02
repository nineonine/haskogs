{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings#-}

module Discogs.Types.Search where

import Discogs.Types.Pagination
import Discogs.Types.Search.Result

import qualified Data.Text as T

import Data.Aeson hiding (Result)
import GHC.Generics

data SearchResult = SearchResult
    { pagination :: Pagination
    , results   :: [Result]
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

type SearchQuery = [(T.Text, T.Text)] --

prepareQuery :: SearchQuery -> T.Text
prepareQuery = T.replace " " "%20" . T.concat . map ( \(a,b) -> T.concat [a , "=" , b, "&"] )

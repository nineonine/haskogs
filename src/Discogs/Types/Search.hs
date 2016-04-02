{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings#-}

module Discogs.Types.Search where

import Discogs.Types.Pagination
import Discogs.Types.Search.Result

import qualified Data.List as L
import Data.Text hiding (map, intersperse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.Aeson hiding (Result)
import GHC.Generics

data Search = Search
    { pagination :: Pagination
    , results   :: [Result]
    } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

type SearchQuery = [(BS.ByteString, BS.ByteString)]

preparedQuery :: SearchQuery -> BS.ByteString
preparedQuery pairs = L.intersperse ("&" :: BS.ByteString) combinedPairs
                      where
                      combinedPairs = map ( \ (a,b) -> BS.concat [a , "=" , b] ) pairs :: [BS.ByteString]

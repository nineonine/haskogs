{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Discogs.Tools where

import Data.Char
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Prelude hiding (concat)
import qualified Data.Text as T
import Data.Text.Read
import Data.Text.Encoding
import Network.HTTP.Client
import Data.Default.Class
import Data.ByteString hiding (dropWhile, map)
import qualified Data.ByteString.Char8 as C


toBS :: T.Text -> ByteString
toBS = encodeUtf8

intToBs :: Int -> ByteString
intToBs = C.pack . show

type Params = [(T.Text, T.Text)]

data OptionalParams = forall a. (Show a, ToJSON a) => OP T.Text a

toOptionalParams :: Params -> [OptionalParams]
toOptionalParams = map toPS
                   where
                   toPS (a, b@"True") = OP a True
                   toPS (a, b@"False") = OP a False
                   toPS (a, b) = case double b of
                                 Right (f, s) -> OP a f
                                 Left _       -> OP a b

toKeyValues :: [OptionalParams] -> [Pair]
toKeyValues = map ( \(OP a b) -> a .= b )

preparePairs :: Params -> [Pair]
preparePairs = toKeyValues . toOptionalParams

-- for setQueryString
mkParams :: Params -> [(ByteString, Maybe ByteString)]
mkParams = map $ \(a,b) -> (,) (encodeUtf8 a) (Just . encodeUtf8 $ b ) -- T.replace " " "%20"


toText :: (Show a) => a -> T.Text
toText s = if Prelude.all isDigit (show s)
             then T.pack (show s)
             else T.pack (read $ show s) :: T.Text

secureReq :: Request
secureReq = def { secure = True
                , port = 443 }


data APIMessage = APIMessage { message :: T.Text } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

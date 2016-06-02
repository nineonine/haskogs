module Discogs.Tools where

import Discogs.Types.Pagination

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

type Params = [(T.Text, T.Text)]
data OptionalParams = forall a. (Show a, ToJSON a) => OP T.Text a
data APIMessage = APIMessage { message :: T.Text } deriving (Show, Read, Eq, Generic)
instance ToJSON APIMessage
instance FromJSON APIMessage

class DiscogsResource resource where
    type ID resource
    resourceId :: resource -> ID resource
    resourceUrl :: resource -> T.Text

class Paginated resource where
    type Content resource
    pagination :: resource -> Maybe Pagination
    contents :: resource -> Content resource

-- functions for preparing optional URL parameters --
toOptionalParams :: Params -> [OptionalParams]
toOptionalParams = map toPS
                   where
                   toPS (a, "True") = OP a True
                   toPS (a, "False") = OP a False
                   toPS (a, b) = case double b of
                                 Right (f, _) -> OP a f
                                 Left _       -> OP a b

toKeyValues :: [OptionalParams] -> [Pair]
toKeyValues = map ( \(OP a b) -> a .= b )

preparePairs :: Params -> [Pair]
preparePairs = toKeyValues . toOptionalParams

-- for setQueryString --
mkParams :: Params -> [(ByteString, Maybe ByteString)]
mkParams = map $ \(a,b) -> (,) (encodeUtf8 a) (Just . encodeUtf8 $ b ) -- T.replace " " "%20"

optParams :: Maybe Params -> [Pair]
optParams ps = case ps of
                Just prms -> preparePairs prms
                _         -> []
--------------------------------------------------------

-- Stringy Types conversions --
toText :: (Show a) => a -> T.Text
toText s = if Prelude.all isDigit (show s)
             then T.pack (show s)
             else T.pack (read $ show s) :: T.Text

toBS :: T.Text -> ByteString
toBS = encodeUtf8

intToBs :: Int -> ByteString
intToBs = C.pack . show
----------------------------

-- for request template with port 443 --
secureReq :: Request
secureReq = def { secure = True
                , port = 443 }
--------------------------------------

-- Useful function for unwraping standard DiscogsT result value --
fromRightMaybe :: Either b (Maybe a) -> a
fromRightMaybe (Right (Just smth) ) = smth
------------------------------------------------------------------

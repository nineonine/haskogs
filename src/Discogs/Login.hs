module Discogs.Login where

import Data.Text hiding (pack)
import Data.Default.Class
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)

-- | Login Method
--   Set to Anonymous by Default
data LoginMethod = Anonymous
                 | Token ByteString -- generated token
                  deriving (Show, Eq)

instance Default LoginMethod where
    def = Anonymous

getToken :: LoginMethod -> ByteString
getToken (Token t) = t
getToken Anonymous = pack "no_token"

module Discogs.Types.Alias where

import           Control.Applicative
import           Data.Aeson
import           Data.Text
import           Data.Maybe (fromJust)
import           Discogs.Tools

data Alias = Alias
    { alias_resource_url :: Text
    , alias_id           :: Maybe Int
    , alias_name         :: Maybe Text
    } deriving (Show, Read, Eq)

instance DiscogsResource Alias where
    type ID Alias = Int
    resourceId    = fromJust . alias_id
    resourceUrl   = alias_resource_url

instance FromJSON Alias where
    parseJSON = withObject "alias" $ \o -> do
        alias_resource_url <- o .: "resource_url"
        alias_id           <- o .:? "id"
        alias_name         <- optional (o .: "name" <|> o .: "username")
        return Alias{..}

instance ToJSON Alias where
    toJSON Alias{..} = object [
        "resource_url" .= alias_resource_url,
        "id"           .= alias_id,
        "name"         .= alias_name]

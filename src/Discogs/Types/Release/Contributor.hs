module Discogs.Types.Release.Contributor where

import Data.Text
import Data.Aeson

data Contributor = Contributor
  { contributor_username     :: Text
  , contributor_resource_url :: Text
  } deriving (Show, Read, Eq)

instance FromJSON Contributor where
    parseJSON = withObject "contributor" $ \o -> do
        contributor_username     <- o .: "username"
        contributor_resource_url <- o .: "resource_url"
        return Contributor{..}

instance ToJSON Contributor where
    toJSON Contributor{..} = object [
        "username"     .= contributor_username,
        "resource_url" .= contributor_resource_url ]

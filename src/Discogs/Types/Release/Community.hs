module Discogs.Types.Release.Community where

import Discogs.Types.Release.Contributor
import Discogs.Types.Release.Rating

import Data.Text
import Data.Aeson

data Community = Community
  { community_status       :: Text
  , community_rating       :: Rating
  , community_want         :: Int
  , community_contributors :: [Contributor]
  , community_have         :: Int
  , community_submitter    :: Contributor
  , community_data_quality :: Text
  } deriving (Show, Read, Eq)

instance FromJSON Community where
    parseJSON = withObject "community" $ \o -> do
        community_status       <- o .: "status"
        community_rating       <- o .: "rating"
        community_want         <- o .: "want"
        community_contributors <- o .: "contributors"
        community_have         <- o .: "have"
        community_submitter    <- o .: "submitter"
        community_data_quality <- o .: "data_quality"
        return Community{..}


instance ToJSON Community where
    toJSON Community{..} = object [
        "status"       .= community_status,
        "rating"       .= community_rating,
        "want"         .= community_want,
        "contributors" .= community_contributors,
        "have"         .= community_have,
        "submitter"    .= community_submitter,
        "data_quality" .= community_data_quality ]

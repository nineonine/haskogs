{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.User.Contribution where

import Discogs.Tools
import Discogs.Types.Pagination
import Discogs.Types.Release

import Data.Aeson hiding (Result)

data Contributions = Contributions {
      contribution_pagination    :: Maybe Pagination
    , contribution_contributions :: [Release]
} deriving (Show, Read, Eq)

instance Paginated Contributions where
    type Content Contributions = [Release]
    pagination = contribution_pagination
    contents = contribution_contributions

instance FromJSON Contributions where
    parseJSON = withObject "searchResult" $ \o -> do
        contribution_pagination      <- o.:? "pagination"
        contribution_contributions   <- o .: "contributions"
        return Contributions{..}

instance ToJSON Contributions where
    toJSON Contributions{..} = object [
        "pagination" .= contribution_pagination ,
        "contributions"   .= contribution_contributions ]

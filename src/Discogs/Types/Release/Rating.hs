{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Release.Rating where

import Data.Aeson

data Rating = Rating
  { rating_count   :: Int
  , rating_average :: Double
  } deriving (Show, Read, Eq)

instance FromJSON Rating where
    parseJSON = withObject "rating" $ \o -> do
        rating_count   <- o .: "count"
        rating_average <- o .: "average"
        return Rating{..}

instance ToJSON Rating where
    toJSON Rating{..} = object [
        "count"   .= rating_count,
        "average" .= rating_average ]

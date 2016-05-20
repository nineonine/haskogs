{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Discogs.Types.Release.Video where

import Data.Text
import Data.Aeson

data Video = Video
      { video_duration    :: Int
      , video_description :: Text
      , video_embed       :: Bool
      , video_uri         :: Text
      , video_title       :: Text
      } deriving (Show, Read, Eq)

instance FromJSON Video where
    parseJSON = withObject "video" $ \o -> do
        video_duration    <- o .: "duration"
        video_description <- o .: "description"
        video_embed       <- o .: "embed"
        video_uri         <- o .: "uri"
        video_title       <- o .: "title"
        return Video{..}

instance ToJSON Video where
    toJSON Video{..} = object [
        "duration"    .= video_duration,
        "description" .= video_description,
        "embed"       .= video_embed,
        "uri"         .= video_uri,
        "title"       .= video_title ]

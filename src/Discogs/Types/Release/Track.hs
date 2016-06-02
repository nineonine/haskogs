module Discogs.Types.Release.Track where

import Data.Text
import Data.Aeson

data Track = Track
    { track_duration :: Text
    , track_position :: Text
    , track_type     :: Text
    , track_title    :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Track where
    parseJSON = withObject "track" $ \o -> do
        track_duration <- o .: "duration"
        track_position <- o .: "position"
        track_type     <- o .: "type_"
        track_title    <- o .: "title"
        return Track{..}

instance ToJSON Track where
    toJSON Track{..} = object [
        "duration" .= track_duration,
        "position" .= track_position,
        "type_"    .= track_type,
        "title"    .= track_title ]

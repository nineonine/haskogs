module Discogs.Types.Release.Identifier where

import Data.Text
import Data.Aeson

data Identifier = Identifier
        { itype       :: Text
        , idescription :: Maybe Text
        , ivalue       :: Text
        } deriving (Show, Read, Eq)

instance FromJSON Identifier where
    parseJSON = withObject "identifier" $ \o -> do
        itype        <- o .: "type"
        idescription <- o .:? "description"
        ivalue       <- o .: "value"
        return Identifier{..}

instance ToJSON Identifier where
    toJSON Identifier{..} = object [
        "type"        .= itype,
        "description" .= idescription,
        "value"       .= ivalue ]

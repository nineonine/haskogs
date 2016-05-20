{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.Artist.Member where

import           Data.Aeson
import           Data.Text

data Member = Member
    { member_active              :: Bool
    , member_resource_url :: Text
    , member_id           :: Int
    , member_name         :: Text
    } deriving (Show, Read, Eq)

instance FromJSON Member where
    parseJSON = withObject "member" $ \o -> do
        member_active              <- o .: "active"
        member_resource_url <- o .: "resource_url"
        member_id           <- o .: "id"
        member_name         <- o .: "name"
        return Member{..}


instance ToJSON Member where
    toJSON Member{..} = object [
        "active"       .= member_active,
        "resource_url" .= member_resource_url,
        "id"           .= member_id,
        "name"         .= member_name ]

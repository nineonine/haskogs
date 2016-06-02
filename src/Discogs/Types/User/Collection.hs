module Discogs.Types.User.Collection where

import Discogs.Types.User.Folder

import Data.Aeson
import GHC.Generics

data Collection = Collection { folders :: [Folder] } deriving (Read, Show, Eq, Generic)

instance ToJSON Collection
instance FromJSON Collection

module Discogs
  ( -- * Re-exports
    module Discogs.Actions
  , module Discogs.Login
  , module Discogs.Types
  , module Discogs.Types.Error
  , module Discogs.Types.Discogs
  , module Discogs.Tools
  ) where

import Discogs.Actions
import Discogs.Login
import Discogs.Types
import Discogs.Types.Error
import Discogs.Types.Discogs
import Discogs.Tools

import Data.ByteString.Lazy (ByteString)

tok :: ByteString
tok = "gdnfNCVuNkjWFzhFBQkZkSOIAPHlmCDwMYnsOAsN"

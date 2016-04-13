module Discogs.Types.Error where

import Network.HTTP.Types
import Control.Exception

-- TODO
-- https://www.discogs.com/developers/#page:home,header:home-quickstart


data DiscogsError = Some SomeException
                  | DiscogsError APIerror
                  | HTTPError Status
                  | ParseError String
                  deriving (Show)

data APIerror   = ResourceNotFound
                  | ServerError
                  | RateLimitError
                  | NotAuthorizedError
                  deriving (Show, Eq)

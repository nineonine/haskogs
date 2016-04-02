module Discogs.Types.Error where

import Network.HTTP.Types

-- TODO
-- https://www.discogs.com/developers/#page:home,header:home-quickstart


data DiscogsError = DiscogsError APIerror
                  | HTTPError Status
                  | ParseError String
                  deriving (Show)

data APIerror   = ResourceNotFound
                  | ServerError
                  | RateLimitError
                  | NotAuthorizedError
                  deriving (Show, Eq)

{-# LANGUAGE OverloadedStrings #-}

module Discogs.Request.MarketPlace where

import Discogs.Tools

import Data.Aeson (object, (.=), encode)
import Prelude hiding (concat)
import Data.Text hiding (concat, pack, append)
import Network.HTTP.Client
import Data.ByteString
import qualified Data.ByteString.Char8 as C

getInventory :: Text -> Request
getInventory user = secureReq { path = concat [ "/users/" , toBS user , "/inventory" ] }

getListing :: Int -> Request
getListing lid = secureReq { path = concat [ "/marketplace/listings/" , intToBs lid ] }

editListing :: Int -> Int -> Text -> Double -> Text -> Maybe Params -> Request
editListing lid rid c p st ps = let reqBody = object $ (optParams ps) ++ [ "release_id" .= rid , "condition" .= c , "price" .= p , "status" .= st ]
                                in secureReq { path = concat [ "/marketplace/listings/" , intToBs lid ]
                                  , method = "POST"
                                  , requestHeaders = [("Content-Type", "application/json")]
                                  , requestBody = RequestBodyLBS $ encode reqBody }

postListing :: Int -> Text -> Double -> Text -> Maybe Params -> Request
postListing rid c p st ps = let reqBody = object $ (optParams ps) ++ [ "release_id" .= rid , "condition" .= c , "price" .= p , "status" .= st ]
                            in secureReq { path = "/marketplace/listings"
                              , method = "POST"
                              , requestHeaders = [("Content-Type", "application/json")]
                              , requestBody = RequestBodyLBS $ encode reqBody }

deleteListing :: Int -> Request
deleteListing lid = secureReq { path = concat [ "/marketplace/listings/" , intToBs lid ]
                        , method = "DELETE"
                        , requestHeaders = [("Content-Type", "application/json")]}

getOrder :: Text -> Request
getOrder oid = secureReq { path = concat [ "/marketplace/orders/" , toBS oid ] }

postOrder :: Text -> Maybe Params -> Request
postOrder oid ps = let reqBody = object $ ("order_id" .= oid) : optParams ps
                    in secureReq { path = concat ["/marketplace/orders/" , toBS oid ]
                      , method = "POST"
                      , requestHeaders = [("Content-Type", "application/json")]
                      , requestBody = RequestBodyLBS $ encode reqBody  }

getListOrders :: Request
getListOrders = secureReq { path = "/marketplace/orders" }

getListOrderMessages :: Text -> Request
getListOrderMessages oid = secureReq { path = concat [ "/marketplace/orders/" , toBS oid , "/messages" ] }

postMessage :: Text -> Maybe Params -> Request
postMessage oid ps = secureReq { path = concat ["/marketplace/orders/" , toBS oid , "/messages"]
                      , method = "POST"
                      , requestHeaders = [("Content-Type", "application/json")]
                      , requestBody = RequestBodyLBS . encode . object $ optParams ps }

getFee :: Double -> Request
getFee price = secureReq { path = concat [ "/marketplace/fee/" , C.pack $ show price ] }

getFeeWithCurrency :: Double -> Text -> Request
getFeeWithCurrency price currency = secureReq { path = concat [ "/marketplace/fee/" , C.pack $ show price , "/" , toBS currency ] }

getPriceSuggestions :: Int -> Request
getPriceSuggestions rid = secureReq { path = concat [ "/marketplace/price_suggestions/" , intToBs rid ] }

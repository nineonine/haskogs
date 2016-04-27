module Discogs.Actions.MarketPlace where

import Discogs.Types.Discogs
import Discogs.Tools

import Data.Aeson
import Data.Text
import Discogs.Types.MarketPlace.Order
import Discogs.Types.MarketPlace.Price
import Discogs.Types.MarketPlace.Listing
import Discogs.Types.MarketPlace.Message
import Discogs.Types.MarketPlace.PriceSuggestion
import qualified Discogs.Request.MarketPlace as Req

inventory :: (Monad m) => Text -> DiscogsT m Inventory
inventory username = runRequest $ Req.getInventory username

listing :: (Monad m) => Int -> DiscogsT m Listing
listing lid = runRequest $ Req.getListing lid

editListing :: (Monad m) => Int -> Int -> Text -> Double -> Text -> Maybe Params -> DiscogsT m ()
editListing lid rid c p st ps = runRequest $ Req.editListing lid rid c p st ps

deleteListing :: (Monad m) => Int -> DiscogsT m Value
deleteListing lid = runRequest $ Req.deleteListing lid

newListing :: (Monad m) => Int -> Text -> Double -> Text -> Maybe Params -> DiscogsT m Value
newListing rid c p st ps = runRequest $ Req.postListing rid c p st ps

order :: (Monad m) => Text -> DiscogsT m Order
order oid = runRequest $ Req.getOrder oid

updateOrder :: (Monad m) => Text -> Maybe Params -> DiscogsT m Order
updateOrder oid ps = runRequest $ Req.postOrder oid ps

listOrders :: (Monad m) => DiscogsT m Orders
listOrders = runRequest Req.getListOrders

listOrderMessages :: (Monad m) => Text -> DiscogsT m Messages
listOrderMessages oid = runRequest $ Req.getListOrderMessages oid

addMessage :: (Monad m) => Text -> Maybe Params -> DiscogsT m Message
addMessage oid ps = runRequest $ Req.postMessage oid ps

fee :: (Monad m) => Double -> DiscogsT m Price
fee pr = runRequest $ Req.getFee pr

feeWithCurrency :: (Monad m) => Double -> Text -> DiscogsT m Price
feeWithCurrency pr cur = runRequest $ Req.getFeeWithCurrency pr cur

priceSuggestions :: (Monad m) => Int -> DiscogsT m PriceSuggestion
priceSuggestions rid = runRequest $ Req.getPriceSuggestions rid

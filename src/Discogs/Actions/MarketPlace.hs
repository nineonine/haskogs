-- | Contains MarketPlace related actions
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

-- | Get a seller’s 'Inventory'
--   If you are not authenticated as the inventory owner, only items that have a status of __For Sale__ will be visible.
--   If you are authenticated as the inventory owner you will get additional /weight/, /format_quantity/, /external_id/, and /location/ keys.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-inventory-get>
inventory :: (Monad m)
          => Text -- ^ The username for whose inventory you are fetching
          -> DiscogsT m Inventory
inventory username = runRequest $ Req.getInventory username

-- | The Listing resource allows you to view Marketplace listings.
--   If the authorized user is the listing owner the listing will include the weight, format_quantity, external_id, and location keys.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-listing-get>
listing :: (Monad m)
        => Int -- ^ The ID of the listing you are fetching
        -> DiscogsT m Listing
listing lid = runRequest $ Req.getListing lid

-- | Edit the data associated with a listing.
--   If the listing’s status is not __For Sale__, __Draft__, or __Expired__, it cannot be modified – __only deleted__.
--   To re-list a Sold listing, a new listing must be created.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-listing-post>
editListing :: (Monad m)
            => Int -- ^ The ID of the listing you are fetching.
            -> Int -- ^ The ID of the release you are posting.
            -> Text -- ^ The condition of the release you are posting.
            -> Double -- ^ The price of the item (in the seller’s currency).
            -> Text -- ^ The status of the listing. Defaults to “For Sale”.
            -> Maybe Params -- ^ Optional params
            -> DiscogsT m ()
editListing lid rid c p st ps = runRequest $ Req.editListing lid rid c p st ps

-- | Permanently remove a listing from the Marketplace.
--   Authentication as the listing owner is required.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-listing-delete>
deleteListing :: (Monad m)
              => Int  -- ^ The ID of the listing you are fetching.
              -> DiscogsT m Value
deleteListing lid = runRequest $ Req.deleteListing lid

-- | Create a Marketplace listing.
--   Authentication is required; the listing will be added to the authenticated user’s Inventory.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-new-listing-post>
newListing :: (Monad m)
           => Int -- ^ The ID of the release you are posting.
           -> Text -- ^ The condition of the release you are posting.
           -> Double -- ^ The price of the item (in the seller’s currency).
           -> Text -- ^ The status of the listing. Defaults to “For Sale”.
           -> Maybe Params -- ^ Optional params
           -> DiscogsT m Value
newListing rid c p st ps = runRequest $ Req.postListing rid c p st ps

-- | View the data associated with an order.
--   Authentication as the seller is required.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-order-get>
order :: (Monad m)
      => Text -- ^ The ID of the order you are fetching
      -> DiscogsT m Order
order oid = runRequest $ Req.getOrder oid

-- | Edit the data associated with an order.
--   Authentication as the seller is required.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-order-post>
updateOrder :: (Monad m)
            => Text -- ^ The ID of the order you are fetching
            -> Maybe Params -- ^ Optional Params
            -> DiscogsT m Order
updateOrder oid ps = runRequest $ Req.postOrder oid ps

-- | Returns a list of the authenticated user’s orders.
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-list-orders-get>
listOrders :: (Monad m) => DiscogsT m Orders
listOrders = runRequest Req.getListOrders

-- | Returns a list of the order’s messages with the most recent first.
--   Accepts Pagination parameters.
---  Authentication as the seller is required.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-list-order-messages-get>
listOrderMessages :: (Monad m)
                  => Text -- ^ The ID of the order you are fetching
                  -> DiscogsT m Messages
listOrderMessages oid = runRequest $ Req.getListOrderMessages oid

-- | Adds a new message to the order’s message log.
--   When posting a new message, you can simultaneously change the order status.
--   While message and status are each optional, one or both must be present.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-list-order-messages-post>
addMessage :: (Monad m) => Text -> Maybe Params -> DiscogsT m Message
addMessage oid ps = runRequest $ Req.postMessage oid ps

-- | Calculate the fee for selling an item on the Marketplace.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-fee-get>
fee :: (Monad m) => Double -> DiscogsT m Price
fee pr = runRequest $ Req.getFee pr

-- | Calculate the fee for selling an item on the Marketplace given a particular currency.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-fee-with-currency-get>
feeWithCurrency :: (Monad m)
                => Double -- ^ The price to calculate a fee from
                -> Text -- ^ may be one of /USD/, /GBP/, /EUR/, /CAD/, /AUD/, or /JPY/. Defaults to /USD/.
                -> DiscogsT m Price
feeWithCurrency pr cur = runRequest $ Req.getFeeWithCurrency pr cur

-- | Retrieve price suggestions for the provided Release ID.
--   <https://www.discogs.com/developers/#page:marketplace,header:marketplace-price-suggestions-get>
priceSuggestions :: (Monad m)
                 => Int -- ^ The release ID to calculate a price from.
                 -> DiscogsT m PriceSuggestion
priceSuggestions rid = runRequest $ Req.getPriceSuggestions rid

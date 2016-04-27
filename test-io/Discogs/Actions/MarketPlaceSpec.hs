{-# LANGUAGE OverloadedStrings #-}

module Discogs.Actions.MarketPlaceSpec where

import qualified Discogs.Types.MarketPlace.Listing as L
import qualified Discogs.Types.MarketPlace.Price as P
import qualified Discogs.Types.MarketPlace.Order as O
import qualified Discogs.Types.MarketPlace.Message as M
import qualified Discogs.Types.MarketPlace.PriceSuggestion as PS

import Discogs.Actions.MarketPlace
import Discogs.Types.Discogs

import Discogs.Tools

import qualified Data.List as List
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

testingToken :: LBS.ByteString
testingToken = "gdnfNCVuNkjWFzhFBQkZkSOIAPHlmCDwMYnsOAsN"

fetchInventoryLength :: IO Int
fetchInventoryLength = do
    inv <- fmap fromRightMaybe $ runDiscogs testingToken $ inventory "chemik_ck"
    return . length $ L.listings inv

fetchlastListingID :: IO Int
fetchlastListingID = do
    inv <- fmap fromRightMaybe $ runDiscogs testingToken $ inventory "chemik_ck"
    return $ List.maximum . map L._id $ L.listings inv

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Actions.MarketPlace" $ do

    it "should fetch inventory by given username" $ do
        resp <- runDiscogs testingToken $ inventory "chemik_ck"
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just inv) ->
                length (L.listings inv) `shouldBe` 1

    it "should create new listing with given required params only" $ do
        resp <- runDiscogs testingToken $ newListing 4941528 "Mint (M)" 100 "Draft" Nothing
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right obj -> obj `shouldNotBe ` Nothing

    it "should delete just created listing" $ do
        lastID <- fetchlastListingID
        resp <- runDiscogs testingToken $ deleteListing lastID
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right _ -> do
                invLen <- fetchInventoryLength
                invLen `shouldBe` 1

    it "should create new listing with given optional params" $ do
        resp <- runDiscogs testingToken $ newListing 4941528 "Mint (M)" 100 "Draft" (Just [("location", "HELLO WORLD")])
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right obj -> do
                lastListingID <- fetchlastListingID
                lastListing <- fmap fromRightMaybe $ runDiscogs testingToken $ listing lastListingID
                L.location lastListing `shouldBe` Just "HELLO WORLD"

    it "should fetch listing by given listing id" $ do
        lastID <- fetchlastListingID
        resp <- runDiscogs testingToken $ listing lastID
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just listing') -> do
                L.location listing' `shouldBe` Just "HELLO WORLD"

    it "should edit listing with given required params only" $ do
        lastID <- fetchlastListingID
        listing' <- fmap fromRightMaybe $ runDiscogs testingToken $ listing lastID
        resp <- runDiscogs testingToken $ editListing (L._id listing') 3477937 "Poor (P)" 50 "Draft" Nothing
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right Nothing -> do -- action successful
                updatedListing <- fmap fromRightMaybe $ runDiscogs testingToken $ listing lastID
                L.condition updatedListing `shouldBe` "Poor (P)"
                L.status updatedListing `shouldBe` "Draft"

    it "should edit listing with given optional params" $ do
        lastID <- fetchlastListingID
        listing' <- fmap fromRightMaybe $ runDiscogs testingToken $ listing lastID
        resp <- runDiscogs testingToken $ editListing (L._id listing') 3477937 "Poor (P)" 50 "Draft" (Just [("location", "BYEBYE WORLD")])
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right Nothing -> do -- action successful
                updatedListing <- fmap fromRightMaybe $ runDiscogs testingToken $ listing (L._id listing')
                L.condition updatedListing `shouldBe` "Poor (P)"
                L.status updatedListing `shouldBe` "Draft"
                L.location updatedListing `shouldBe` Just "BYEBYE WORLD"

    it "should delete listing by given listing id" $ do
        lastID <- fetchlastListingID
        resp <- runDiscogs testingToken $ deleteListing lastID
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right _ -> do
                invLen <- fetchInventoryLength
                invLen `shouldBe` 1

    it "should fetch order by given order id" $ do
        r <- runDiscogs testingToken $ order "545705-1"
        r `shouldSatisfy` isRight
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just o) -> (O._id o) `shouldBe` "545705-1"

    it "should update order shipping by given order id and params" $ do
        r <- runDiscogs testingToken $ updateOrder "545705-1" ( Just [("shipping", "50.0")] )
        r `shouldSatisfy` isRight
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just o) -> (P.value $ O.shipping o) `shouldBe` 50.0

    it "should list orders of authenticated user" $ do
        os <- runDiscogs testingToken $ listOrders
        os `shouldSatisfy` isRight
        case os of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just ordrs) -> (length $ O.orders ordrs) `shouldSatisfy` (>0)

    it "should fetch all messages of given order" $ do
        msgs <- runDiscogs testingToken $ listOrderMessages "545705-1"
        msgs `shouldSatisfy` isRight
        case msgs of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just mgs) -> (length $ M.messages mgs) `shouldSatisfy` (>0)

    it "should add message to given order" $ do
        msg <- runDiscogs testingToken $ addMessage "545705-1" ( Just [("message", "message from hspec")] )
        msg `shouldSatisfy` isRight
        case msg of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just messg) -> M.message messg `shouldBe` "message from hspec"

    it "should fetch calculated fee for selling an item" $ do
        r <- runDiscogs testingToken $ fee 1.0
        r `shouldSatisfy` isRight
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just p) -> (P.value p) `shouldSatisfy` (> 0)

    it "should fetch calculated fee with specified currency for selling an item" $ do
        r <- runDiscogs testingToken $ feeWithCurrency 1.0 "GBP"
        r `shouldSatisfy` isRight
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just p) -> (P.value p) `shouldSatisfy` (> 0)

    it "should fetch all possible price suggestions for given release" $ do
        r <- runDiscogs testingToken $ priceSuggestions 1
        r `shouldSatisfy` isRight
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just ps) -> (P.value $ PS.fair ps) `shouldSatisfy` (> 0)

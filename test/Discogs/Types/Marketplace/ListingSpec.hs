module Discogs.Types.Marketplace.ListingSpec where

import Discogs.Types.Marketplace.Listing

import Data.Aeson (eitherDecode)
import  qualified Data.ByteString.Lazy as LBS
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Types.MarketPlace.Listing" $ do
    describe "Inventory" $ do
        invExample <- runIO $ LBS.readFile "test/data/marketplace/inventory_example.json"

        it "can read inventory example json file" $
            invExample `shouldSatisfy` not . LBS.null

        it "can parse inventory from json" $ do
            let decoded = eitherDecode invExample :: Either String Inventory
            decoded `shouldSatisfy` isRight

    describe "Listing" $ do
        listingExample <- runIO $ LBS.readFile "test/data/marketplace/listing_example.json"

        it "can read listing example json file" $
            listingExample `shouldSatisfy` not . LBS.null

        it "can parse listing from json" $ do
            let decoded = eitherDecode listingExample :: Either String Listing
            decoded `shouldSatisfy` isRight

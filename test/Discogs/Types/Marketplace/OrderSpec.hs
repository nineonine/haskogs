module Discogs.Types.Marketplace.OrderSpec where

import Discogs.Types.Marketplace.Order

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
spec = describe "Discogs.Types.MarketPlace.Order" $ do
    describe "Order" $ do
        orderExample <- runIO $ LBS.readFile "test/data/marketplace/order_example.json"

        it "can read Order example json file" $
            orderExample `shouldSatisfy` not . LBS.null

        it "can parse Order from json" $ do
            let decoded = eitherDecode orderExample :: Either String Order
            decoded `shouldSatisfy` isRight

    describe "Orders" $ do
        ordersExample <- runIO $ LBS.readFile "test/data/marketplace/orders_example.json"

        it "can read Orders example json file" $
            ordersExample `shouldSatisfy` not . LBS.null

        it "can parse Orders from json" $ do
            let decoded = eitherDecode ordersExample :: Either String Orders
            decoded `shouldSatisfy` isRight

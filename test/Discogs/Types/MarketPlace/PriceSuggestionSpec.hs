module Discogs.Types.MarketPlace.PriceSuggestionSpec where

import Discogs.Types.MarketPlace.PriceSuggestion

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
spec = describe "Discogs.Types.MarketPlace.PriceSuggestion" $ do
    describe "PriceSuggestion" $ do
        prcExample <- runIO $ LBS.readFile "test/data/marketplace/price_suggestion_example.json"

        it "can read price suggestion example json file" $
            prcExample `shouldSatisfy` not . LBS.null

        it "can parse price suggestion from json" $ do
            let decoded = eitherDecode prcExample :: Either String PriceSuggestion
            decoded `shouldSatisfy` isRight

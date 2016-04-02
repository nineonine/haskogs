module Discogs.Types.SearchSpec where

import Discogs.Types.Search

import Data.Aeson (eitherDecode)
import  qualified Data.ByteString.Lazy as LBS
import Test.Hspec hiding (example)

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Types.SearchResult" $ do
    describe "SearchResult" $ do
        example <- runIO $ LBS.readFile "test/data/search_example.json"

        it "can read search results example json file" $
            example `shouldSatisfy` not . LBS.null

        it "can parse search results from json" $ do
            let decoded = eitherDecode example :: Either String SearchResult
            decoded `shouldSatisfy` isRight

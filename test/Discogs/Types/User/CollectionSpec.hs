module Discogs.Types.User.CollectionSpec where

import Discogs.Types.User.Collection

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
spec = describe "Discogs.Types.User.Collection" $ do
    describe "Collection" $ do
        collExample <- runIO $ LBS.readFile "test/data/user/collection_example.json"

        it "can read Collection example json file" $
            collExample `shouldSatisfy` not . LBS.null

        it "can parse Collection from json" $ do
            let decoded = eitherDecode collExample :: Either String Collection
            decoded `shouldSatisfy` isRight

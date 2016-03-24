module Discogs.Types.Release.RatingSpec where

import Discogs.Types.Release.Rating

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
spec = describe "Discogs.Types.Release.Rating" $ do
    describe "Video" $ do
        ratingExample <- runIO $ LBS.readFile "test/data/release/rating_example.json"

        it "can read rating example json file" $
            ratingExample `shouldSatisfy` not . LBS.null

        it "can parse rating from json" $ do
            let decoded = eitherDecode ratingExample :: Either String Rating
            decoded `shouldSatisfy` isRight

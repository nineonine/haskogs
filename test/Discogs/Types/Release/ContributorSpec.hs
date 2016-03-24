module Discogs.Types.Release.ContributorSpec where

import Discogs.Types.Release.Contributor

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
spec = describe "Discogs.Types.Release.Contributor" $ do
    describe "Contributor" $ do
        contributorExample <- runIO $ LBS.readFile "test/data/release/contributor_example.json"

        it "can read contributor example json file" $
            contributorExample `shouldSatisfy` not . LBS.null

        it "can parse contributor from json" $ do
            let decoded = eitherDecode contributorExample :: Either String Contributor
            decoded `shouldSatisfy` isRight

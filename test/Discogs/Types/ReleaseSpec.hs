module Discogs.Types.ReleaseSpec where

import Discogs.Types.Release

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
spec = describe "Discogs.Types.Release" $ do
    describe "Release" $ do
        releaseExample <- runIO $ LBS.readFile "test/data/release_example.json"

        it "can read release example json file" $
            releaseExample `shouldSatisfy` not . LBS.null

        it "can parse release from json" $ do
            let decoded = eitherDecode releaseExample :: Either String Release
            decoded `shouldSatisfy` isRight

module Discogs.Types.Label.ReleaseSpec where

import Discogs.Types.Label.Release

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
spec = describe "Discogs.Types.Label.Release" $ do
    describe "LabelRelease" $ do
        labelExample <- runIO $ LBS.readFile "test/data/label/label_release_example.json"

        it "can read label release example json file" $
            labelExample `shouldSatisfy` not . LBS.null

        it "can parse label release from json" $ do
            let decoded = eitherDecode labelExample :: Either String Release
            decoded `shouldSatisfy` isRight

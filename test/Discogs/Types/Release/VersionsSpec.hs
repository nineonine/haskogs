module Discogs.Types.Release.VersionsSpec where

import Discogs.Types.Release.Versions

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
spec = describe "Discogs.Types.Release.Versions" $ do
    describe "Versions" $ do
        verExample <- runIO $ LBS.readFile "test/data/release/release_version_example.json"

        it "can read release versions example json file" $
            verExample `shouldSatisfy` not . LBS.null

        it "can parse release versions from json" $ do
            let decoded = eitherDecode verExample :: Either String Versions
            decoded `shouldSatisfy` isRight

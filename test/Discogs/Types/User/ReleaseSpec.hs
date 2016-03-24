module Discogs.Types.User.ReleaseSpec where

import Discogs.Types.User.Release

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
spec = describe "Discogs.Types.User.Release" $ do
    describe "Releases" $ do
        relExample <- runIO $ LBS.readFile "test/data/user/releases_example.json"

        it "can read Releases example json file" $
            relExample `shouldSatisfy` not . LBS.null

        it "can parse Releases from json" $ do
            let decoded = eitherDecode relExample :: Either String Releases
            decoded `shouldSatisfy` isRight

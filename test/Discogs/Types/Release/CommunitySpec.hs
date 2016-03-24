module Discogs.Types.Release.CommunitySpec where

import Discogs.Types.Release.Community

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
spec = describe "Discogs.Types.Release.Community" $ do
    describe "Community" $ do
        communityExample <- runIO $ LBS.readFile "test/data/release/community_example.json"

        it "can read community example json file" $
            communityExample `shouldSatisfy` not . LBS.null

        it "can parse community from json" $ do
            let decoded = eitherDecode communityExample :: Either String Community
            decoded `shouldSatisfy` isRight

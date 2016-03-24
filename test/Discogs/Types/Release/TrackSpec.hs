module Discogs.Types.Release.TrackSpec where

import Discogs.Types.Release.Track

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
spec = describe "Discogs.Types.Release.Track" $ do
    describe "Track" $ do
        trackExample <- runIO $ LBS.readFile "test/data/release/track_example.json"

        it "can read track example json file" $
            trackExample `shouldSatisfy` not . LBS.null

        it "can parse track from json" $ do
            let decoded = eitherDecode trackExample :: Either String Track
            decoded `shouldSatisfy` isRight

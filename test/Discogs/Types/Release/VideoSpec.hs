module Discogs.Types.Release.VideoSpec where

import Discogs.Types.Release.Video

import Discogs.TestTools

import Data.Aeson (eitherDecode)
import  qualified Data.ByteString.Lazy as LBS
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Types.Release.Video" $ do
    describe "Video" $ do
        videoExample <- runIO $ LBS.readFile "test/data/release/video_example.json"

        it "can read video example json file" $
            videoExample `shouldSatisfy` not . LBS.null

        it "can parse a video from json" $ do
            let decoded = eitherDecode videoExample :: Either String Video
            decoded `shouldSatisfy` isRight

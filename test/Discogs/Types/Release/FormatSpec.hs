module Discogs.Types.Release.FormatSpec where

import Discogs.Types.Release.Format

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
spec = describe "Discogs.Types.Release.Format" $ do
    describe "Format" $ do
        formatExample <- runIO $ LBS.readFile "test/data/release/format_example.json"

        it "can read format example json file" $
            formatExample `shouldSatisfy` not . LBS.null

        it "can parse format from json" $ do
            let decoded = eitherDecode formatExample :: Either String Format
            decoded `shouldSatisfy` isRight

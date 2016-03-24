module Discogs.Types.Release.MasterSpec where

import Discogs.Types.Release.Master

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
spec = describe "Discogs.Types.Release.Master" $ do
    describe "Master" $ do
        masterExample <- runIO $ LBS.readFile "test/data/release/master_example.json"

        it "can read master example json file" $
            masterExample `shouldSatisfy` not . LBS.null

        it "can parse master from json" $ do
            let decoded = eitherDecode masterExample :: Either String Master
            decoded `shouldSatisfy` isRight

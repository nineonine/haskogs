module Discogs.Types.LabelSpec where

import Discogs.Types.Label

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
spec = describe "Discogs.Types.Label" $ do
    describe "Label" $ do
        labelExample <- runIO $ LBS.readFile "test/data/label_example.json"

        it "can read label example json file" $
            labelExample `shouldSatisfy` not . LBS.null

        it "can parse label from json" $ do
            let decoded = eitherDecode labelExample :: Either String Label
            decoded `shouldSatisfy` isRight

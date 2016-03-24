module Discogs.Types.Release.IdentifierSpec where

import Discogs.Types.Release.Identifier

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
spec = describe "Discogs.Types.Release.Identifier" $ do
    describe "Identifier" $ do
        identifierExample <- runIO $ LBS.readFile "test/data/release/identifier_example.json"

        it "can read identifier example json file" $
            identifierExample `shouldSatisfy` not . LBS.null

        it "can parse identifier from json" $ do
            let decoded = eitherDecode identifierExample :: Either String Identifier
            decoded `shouldSatisfy` isRight

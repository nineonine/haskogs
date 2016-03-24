module Discogs.Types.User.WantSpec where

import Discogs.Types.User.Want

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
spec = describe "Discogs.Types.User.Want" $ do
    describe "Wants" $ do
        wantExample <- runIO $ LBS.readFile "test/data/user/wants_example.json"

        it "can read Wants example json file" $
            wantExample `shouldSatisfy` not . LBS.null

        it "can parse Wants from json" $ do
            let decoded = eitherDecode wantExample :: Either String Wants
            decoded `shouldSatisfy` isRight

module Discogs.Types.User.FieldSpec where

import Discogs.Types.User.Field

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
spec = describe "Discogs.Types.User.Field" $ do
    describe "Fields" $ do
        fExample <- runIO $ LBS.readFile "test/data/user/fields_example.json"

        it "can read Fields example json file" $
            fExample `shouldSatisfy` not . LBS.null

        it "can parse Fields from json" $ do
            let decoded = eitherDecode fExample :: Either String Fields
            decoded `shouldSatisfy` isRight

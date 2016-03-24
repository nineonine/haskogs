module Discogs.Types.UserSpec where

import Discogs.Types.User

import Data.Aeson (eitherDecode)
import  qualified Data.ByteString.Lazy as LBS
import Test.Hspec hiding (example)

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Types.User" $ do
    describe "User" $ do
        example <- runIO $ LBS.readFile "test/data/user_example.json"

        it "can read User example json file" $
            example `shouldSatisfy` not . LBS.null

        it "can parse User from json" $ do
            let decoded = eitherDecode example :: Either String User
            decoded `shouldSatisfy` isRight

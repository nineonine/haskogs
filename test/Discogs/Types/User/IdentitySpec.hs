module Discogs.Types.User.IdentitySpec where

import Discogs.Types.User.Identity

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
spec = describe "Discogs.Types.User.Identity" $ do
    describe "Identity" $ do
        idExample <- runIO $ LBS.readFile "test/data/user/identity_example.json"

        it "can read identity example json file" $
            idExample `shouldSatisfy` not . LBS.null

        it "can parse identity from json" $ do
            let decoded = eitherDecode idExample :: Either String Identity
            decoded `shouldSatisfy` isRight

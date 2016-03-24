module Discogs.Types.AliasSpec where

import Discogs.Types.Alias

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
spec = describe "Discogs.Types.Alias" $ do
    describe "Alias" $ do
        aliasExample <- runIO $ LBS.readFile "test/data/alias_example.json"

        it "can read artist example json file" $
            aliasExample `shouldSatisfy` not . LBS.null

        it "can parse artist from json" $ do
            let decoded = eitherDecode aliasExample :: Either String Alias
            decoded `shouldSatisfy` isRight

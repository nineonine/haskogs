module Discogs.Types.User.ContributionSpec where

import Discogs.Types.User.Contribution

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
spec = describe "Discogs.Types.User.Contribution" $ do
    describe "Contributions" $ do
        contexample <- runIO $ LBS.readFile "test/data/user/contributions_example.json"

        it "can read Contribution example json file" $
            contexample `shouldSatisfy` not . LBS.null

        it "can parse Contribution from json" $ do
            let decoded = eitherDecode contexample :: Either String Contributions
            decoded `shouldSatisfy` isRight

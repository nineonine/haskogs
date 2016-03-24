module Discogs.Types.Release.CompanySpec where

import Discogs.Types.Release.Company

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
spec = describe "Discogs.Types.Release.Company" $ do
    describe "Company" $ do
        companyExample <- runIO $ LBS.readFile "test/data/release/company_example.json"

        it "can read company example json file" $
            companyExample `shouldSatisfy` not . LBS.null

        it "can parse company from json" $ do
            let decoded = eitherDecode companyExample :: Either String Company
            decoded `shouldSatisfy` isRight

module Discogs.Types.PaginationSpec where

import Discogs.Types.Pagination

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
spec = describe "Discogs.Types.Pagination" $ do
    describe "Image" $ do
        pagExample <- runIO $ LBS.readFile "test/data/pagination_example.json"

        it "can read pagination example json file" $
            pagExample `shouldSatisfy` not . LBS.null

        it "can parse pagination from json" $ do
            let decoded = eitherDecode pagExample :: Either String Pagination
            decoded `shouldSatisfy` isRight

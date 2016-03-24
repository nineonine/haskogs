module Discogs.Types.ImageSpec where

import Discogs.Types.Image

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
spec = describe "Discogs.Types.Image" $ do
    describe "Image" $ do
        imageExample <- runIO $ LBS.readFile "test/data/image_example.json"

        it "can read image example json file" $
            imageExample `shouldSatisfy` not . LBS.null

        it "can parse image from json" $ do
            let decoded = eitherDecode imageExample :: Either String Image
            decoded `shouldSatisfy` isRight

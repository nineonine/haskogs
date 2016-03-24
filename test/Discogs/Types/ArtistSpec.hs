module Discogs.Types.ArtistSpec where

import Discogs.Types.Artist

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
spec = describe "Discogs.Types.Artist" $ do
    describe "Artist" $ do
        artistExample <- runIO $ LBS.readFile "test/data/artist_example.json"

        it "can read artist example json file" $
            artistExample `shouldSatisfy` not . LBS.null

        it "can parse artist from json" $ do
            let decoded = eitherDecode artistExample :: Either String Artist
            decoded `shouldSatisfy` isRight

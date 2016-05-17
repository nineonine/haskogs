module Discogs.Types.Release.ArtistSpec where

import Discogs.Types.Release.Artist

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
spec = describe "Discogs.Types.Release.Artist" $ do
    describe "Artist" $ do
        raExample <- runIO $ LBS.readFile "test/data/release/artist_example.json"

        it "can read artist example json file" $
            raExample `shouldSatisfy` not . LBS.null

        it "can parse artist from json" $ do
            let decoded = eitherDecode raExample :: Either String ReleaseArtist
            decoded `shouldSatisfy` isRight

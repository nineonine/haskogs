module Discogs.Types.Artist.ReleaseSpec where

import Discogs.Types.Artist.Release

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
spec = describe "Discogs.Types.Artist.Release" $ do
    describe "ArtistRelease" $ do
        arExample <- runIO $ LBS.readFile "test/data/artist/artist_release_example.json"

        it "can read artist release example json file" $
            arExample `shouldSatisfy` not . LBS.null

        it "can parse artist release from json" $ do
            let decoded = eitherDecode arExample :: Either String Release
            decoded `shouldSatisfy` isRight

    describe "ArtistReleases" $ do
        arsExample <- runIO $ LBS.readFile "test/data/artist/artist_releases_example.json"

        it "can read artist releases example json file" $
            arsExample `shouldSatisfy` not . LBS.null

        it "can parse artist releases from json" $ do
            let decoded = eitherDecode arsExample :: Either String ArtistReleases
            decoded `shouldSatisfy` isRight

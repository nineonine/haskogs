{-# LANGUAGE OverloadedStrings #-}

module Discogs.Actions.DatabaseSpec where

import qualified Discogs.Types.Release as R
import qualified Discogs.Types.Artist as A
import qualified Discogs.Types.Label as L
import qualified Discogs.Types.Search as S
import qualified Discogs.Types.Release.Master as M
import qualified Discogs.Types.Release.Versions as RV
import qualified Discogs.Types.Artist.Release as AR
import qualified Discogs.Types.Label.Release as LR
import qualified Discogs.Types.Pagination as P

import Discogs.Actions.Database
import Discogs.Types.Discogs
import qualified Data.Text as T

import  qualified Data.ByteString.Lazy as LBS
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

testingToken :: LBS.ByteString
testingToken = "gdnfNCVuNkjWFzhFBQkZkSOIAPHlmCDwMYnsOAsN"

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Actions.Database" $ do

    it "should be able to get release 1" $ do
        resp <- runDiscogsAnon $ release 1
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right r ->
                R.title r `shouldBe` "Stockholm"

    it "should be able to get artist" $ do
        resp <- runDiscogsAnon $ artist 1
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right a ->
                A.name a `shouldBe` "The Persuader"

    it "should be able to get master 1001" $ do
        resp <- runDiscogsAnon $ master 1001
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right m ->
                M.title m `shouldBe` "Diese Momente EP"

    it "should be able to get label" $ do
        resp <- runDiscogsAnon $ label 1001
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right l ->
                L.name l `shouldBe` "Muzik Magazine"

    it "should be able to get all master 150 release versions" $ do
        resp <- runDiscogsAnon $ releaseVersions 150
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right rv ->
                (length $ RV.versions rv) `shouldBe` 4

    it "should be able to get all artist releases" $ do
        resp <- runDiscogsAnon $ artistReleases 1020
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ar ->
                (length $ AR.releases ar) `shouldBe` 9

    it "should be able to get all label releases" $ do
        resp <- runDiscogsAnon $ labelReleases 50
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right lr ->
                (length $ LR.releases lr) `shouldBe` 34

    it "should be able to perform search" $ do
        resp <- runDiscogs testingToken $ search [("artist", "robert hood")]
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right sr ->
                (P.items $ S.pagination sr) `shouldSatisfy` (==248)

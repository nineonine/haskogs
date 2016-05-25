{-# LANGUAGE OverloadedStrings #-}

module Discogs.Actions.DatabaseSpec where

import Discogs.Types
import Discogs.Types.Discogs

import Discogs.Actions.Database

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
            Right (Just r) ->
                release_title r `shouldBe` "Stockholm"

    it "should be able to get artist" $ do
        resp <- runDiscogsAnon $ artist 1
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just a) ->
                artist_name a `shouldBe` "The Persuader"

    it "should be able to get master 1001" $ do
        resp <- runDiscogsAnon $ master 1001
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just m) ->
                master_title m `shouldBe` "Diese Momente EP"

    it "should be able to get label" $ do
        resp <- runDiscogsAnon $ label 1001
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just l) ->
                label_name l `shouldBe` "Muzik Magazine"

    it "should be able to get all master 150 release versions" $ do
        resp <- runDiscogsAnon $ releaseVersions 150
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just rv) ->
                length (versions_versions rv) `shouldBe` 4

    it "should be able to get all artist releases" $ do
        resp <- runDiscogsAnon $ artistReleases 1020
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just ar) ->
                length (ar_releases ar) `shouldBe` 9

    it "should be able to get all label releases" $ do
        resp <- runDiscogsAnon $ labelReleases 50
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just lr) ->
                length (lr_releases lr) `shouldBe` 35

    it "should be able to perform search" $ do
        resp <- runDiscogs testingToken $ search [("artist", "robert hood")]
        resp `shouldSatisfy` isRight
        case resp of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right (Just sr) ->
                page (sr_pagination sr) `shouldSatisfy` (==1)

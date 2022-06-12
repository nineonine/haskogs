{-# LANGUAGE OverloadedStrings #-}

module Discogs.Actions.UserSpec where

import Discogs.Types
import Discogs.Types.Discogs

import Discogs.Actions.User

import Discogs.Tools

import Data.Text hiding (head, filter, length)
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

testingToken :: LBS.ByteString
testingToken = "gdnfNCVuNkjWFzhFBQkZkSOIAPHlmCDwMYnsOAsN"

testingUser :: Text
testingUser = "chemik_ck"

testingReleaseId :: Int
testingReleaseId = 7000

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Discogs.Actions.User" $ do


    -- IDENTITY SECTION --

    it "should get identity" $ do
        r <- runDiscogs testingToken identity
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just identity ) -> do
                id_username identity `shouldBe` "chemik_ck"

    it "should return user profile" $ do
        r <- runDiscogs testingToken $ profile testingUser
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just prfl ) -> do
                user_email prfl `shouldBe` Just "mail4chemik@gmail.com"

    it "should edit profile" $ do
        r <- runDiscogs testingToken $ editProfile testingUser $ Just [("profile", "updated")]
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just prfl ) -> do
                user_profile prfl `shouldBe` "updated"

    it "should return user submissions" $ do
        r <- runDiscogs testingToken $ submissions testingUser
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just smbs ) -> do
                labels (subs_submissions smbs) `shouldBe` Nothing

    it "should return user contributions" $ do
        r <- runDiscogs testingToken $ contributions testingUser
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just cntrs ) -> do
                contribution_contributions cntrs `shouldBe` []


    -- COLLECTION SECTION --

    it "should get user collection" $ do
        r <- runDiscogs testingToken $ collection testingUser
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just collctn ) -> do
                Prelude.length (folders collctn ) `shouldBe` 5

    it "should create a folder" $ do
        r <- runDiscogs testingToken $ createFolder testingUser "specFolder"
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just fldr) -> do
                folder_name fldr `shouldBe` "specFolder"

    it "should get specific folder" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "specFolder" ) $ folders cllctn
        r <- runDiscogs testingToken $ folder testingUser $ folder_id fldr
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just fld ) -> do
                folder_name fld `shouldBe` "specFolder"

    it "should edit folder metadata" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "specFolder" ) $ folders cllctn
        r <- runDiscogs testingToken $ editFolder testingUser (folder_id fldr) $ Just [("name", "renamedSpecFolder")]
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just fld ) -> do
                folder_name fld `shouldBe` "renamedSpecFolder"

    it "should add release to specified folder" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "renamedSpecFolder" ) $ folders cllctn
        r <- runDiscogs testingToken $ addReleaseToFolder testingUser (folder_id fldr) testingReleaseId
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right resp -> do
                resp `shouldNotBe` Nothing

    it "should get collection releases" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "renamedSpecFolder" ) $ folders cllctn
        r <- runDiscogs testingToken $ collectionReleases testingUser (folder_id fldr) Nothing
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just collRess ) -> do
                length (ur_releases collRess) `shouldBe` 1

    it "should rate release" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "renamedSpecFolder" ) $ folders cllctn
        Right ( Just rlss ) <- runDiscogs testingToken $ collectionReleases testingUser (folder_id fldr) Nothing
        let Just instanceID = ur_instance_id . head $ ur_releases rlss
        r <- runDiscogs testingToken $ rateRelease testingUser (folder_id fldr) testingReleaseId instanceID $ Just [("rating", "5")]
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right result -> result `shouldBe` Nothing

    it "should delete release from specified folder" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "renamedSpecFolder" ) $ folders cllctn
        Right ( Just rlss ) <- runDiscogs testingToken $ collectionReleases testingUser (folder_id fldr) Nothing
        let Just instanceID = ur_instance_id . head $ ur_releases rlss
        r <- runDiscogs testingToken $ deleteReleaseFromFolder testingUser (folder_id fldr) testingReleaseId instanceID
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right result -> result `shouldBe` Nothing

    it "delete specified folder" $ do
        Right ( Just cllctn ) <- runDiscogs testingToken $ collection testingUser
        let fldr = head $ filter ( \ fld -> folder_name fld == "renamedSpecFolder" ) $ folders cllctn
        r <- runDiscogs testingToken $ deleteFolder testingUser (folder_id fldr)
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right result -> do
                result `shouldBe` Nothing


    it "should get collection notes fields" $ do
        r <- runDiscogs testingToken $ collectionNotesFields testingUser
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right result -> result `shouldNotBe` Nothing


    -- WANTLIST SECTION --

    it "should get user wantlist" $ do
        r <- runDiscogs testingToken $ wantlist testingUser
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right result -> result `shouldNotBe` Nothing

    it "should add release to user's wantlist" $ do
        r <- runDiscogs testingToken $ addToWantlist testingUser testingReleaseId Nothing
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right result -> result `shouldNotBe` Nothing

    it "should edit release's notes and rating in user's wantlist" $ do
        r <- runDiscogs testingToken $ editReleaseInWantlist testingUser testingReleaseId $ Just [("notes", "new note"), ("rating", "3")]
        case r of
            Left _ -> expectationFailure "Request failed. Something went wrong."
            Right ( Just rel ) -> do
                ur_notes rel `shouldBe` (Just "new note")
                ur_rating rel `shouldBe` 3

    it "should remove release from user's wantlist" $ do
        pending
        -- r <- runDiscogs testingToken $ removeFromWantlist testingUser testingReleaseId
        -- case r of
        --     Left _ -> expectationFailure "Request failed. Something went wrong."
        --     Right result -> result `shouldBe` (Nothing)

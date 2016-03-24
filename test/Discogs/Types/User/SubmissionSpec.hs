module Discogs.Types.User.SubmissionSpec where

import Discogs.Types.User.Submission

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
spec = describe "Discogs.Types.User.Submission" $ do
    describe "Submissions" $ do
        submExample <- runIO $ LBS.readFile "test/data/user/submissions_example.json"

        it "can read Submissions example json file" $
            submExample `shouldSatisfy` not . LBS.null

        it "can parse Submissions from json" $ do
            let decoded = eitherDecode submExample :: Either String Submissions
            decoded `shouldSatisfy` isRight

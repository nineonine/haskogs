module Discogs.Types.Artist.MemberSpec where

import Discogs.Types.Artist.Member

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
spec = describe "Discogs.Types.Artist.Member" $ do
    describe "Member" $ do
        memberExample <- runIO $ LBS.readFile "test/data/artist/member_example.json"

        it "can read member example json file" $
            memberExample `shouldSatisfy` not . LBS.null

        it "can parse member from json" $ do
            let decoded = eitherDecode memberExample :: Either String Member
            decoded `shouldSatisfy` isRight

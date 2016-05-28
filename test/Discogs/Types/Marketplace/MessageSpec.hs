module Discogs.Types.Marketplace.MessageSpec where

import Discogs.Types.Marketplace.Message

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
spec = describe "Discogs.Types.MarketPlace.Message" $ do
    describe "Message" $ do
        msgExample <- runIO $ LBS.readFile "test/data/marketplace/message_example.json"

        it "can read Message example json file" $
            msgExample `shouldSatisfy` not . LBS.null

        it "can parse Message from json" $ do
            let decoded = eitherDecode msgExample :: Either String Message
            decoded `shouldSatisfy` isRight

        describe "Messages" $ do
            msgsExample <- runIO $ LBS.readFile "test/data/marketplace/messages_example.json"

            it "can read Messages example json file" $
                msgsExample `shouldSatisfy` not . LBS.null

            it "can parse Messages from json" $ do
                let decoded = eitherDecode msgsExample :: Either String Messages
                decoded `shouldSatisfy` isRight

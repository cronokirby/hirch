{-# LANGUAGE OverloadedStrings #-}

import           Data.List.NonEmpty (NonEmpty (..))
import           IrcTypes
import           Test.Hspec

main :: IO ()
main = hspec $ do
    ircTypesSpec

ircTypesSpec = do
    joinNonEmptySpec


joinNonEmptySpec = describe "joinNonEmpty" $ do
    it "returns an empty string for an empty sep and list elements" $ do
        joinNonEmpty "" ("" :| []) `shouldBe` ""
        joinNonEmpty "" ("" :| ["", ""]) `shouldBe` ""
    it "does nothing for a single element" $ do
        joinNonEmpty "," ("foo" :| []) `shouldBe` "foo"
    it "can seperate by commas" $ do
        joinNonEmpty "," ("1" :| ["2"]) `shouldBe` "1,2"
        joinNonEmpty "," ("1" :| ["2", "3"]) `shouldBe` "1,2,3"


joinIrcChannelsSpec = describe "joinIrcChannels" $ do
    it "simply adds a # for a single channel" $ do
        joinIrcChannels ("foo" :| []) `shouldBe` "#foo"
    it "handles multiple channels" $ do
        joinIrcChannels ("foo" :| ["bar", "baz"]) `shouldBe` "#foo,#bar,#baz"

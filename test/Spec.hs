{-# LANGUAGE OverloadedStrings #-}

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text          as T
import           IrcTypes
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
    ircTypesSpec

ircTypesSpec = do
    joinNonEmptySpec
    joinIrcChannelsSpec
    ircMessageSpec


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


instance Arbitrary IrcMessage where
    arbitrary = do
        t1 <- textGen
        t2 <- textGen
        (joinT : joinTs) <- listOf1 channelGen
        (partT : partTs) <- listOf1 channelGen
        elements [Ping t1, Ping t2, Join (joinT :| joinTs), Part (partT :| partTs)]
      where
        textGen = T.pack . getPrintableString <$> arbitrary
        -- channels can't be empty or contain , or #
        channelGen = T.cons 'a' . T.filter (not . (`elem` [',', '#'])) <$> textGen


ircMessageSpec = describe "readText" $ do
    it "is inverse to sendText" $ property $ \x ->
        (readText . sendText) x == Right (x :: IrcMessage)

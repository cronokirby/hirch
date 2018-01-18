{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text (parseOnly)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.Text            as T
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
    sourceSpec
    serverMessageSpec


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


ircMessageSpec = describe "ircMessage" $ do
    it "is inverse to sendText" $ property $ \x ->
        (parseOnly ircMessage . sendText) x == Right (x :: IrcMessage)

sourceSpec = describe "source" $ do
    it "can parse servers" $ do
        doParse ":a.b.c" `shouldBe` Right (Server "a.b.c")
        doParse ":wilhelm.freenode.net" `shouldBe` Right (Server "wilhelm.freenode.net")
    it "can parse users" $ do
        doParse ":CK!ck@host.net" `shouldBe` Right (User "CK" "ck" "host.net")
  where
    doParse = parseOnly source

serverMessageSpec = describe "serverMessage" $ do
    it "can parse servers" $ do
        doParse ":a.b.c PING :message" `shouldBe` Right (Just (Server "a.b.c"), Ping "message")
        doParse ":a.b.c PONG :message" `shouldBe` Right (Just (Server "a.b.c"), Pong "message")
    it "can parse users" $ do
        doParse ":CK!ck@host.net PING :message" `shouldBe` Right (Just (User "CK" "ck" "host.net"), Ping "message")
    it "can parse no host" $ do
        doParse "PING :message" `shouldBe` Right (Nothing, Ping "message")
    it "can parse MOTD" $ do
        doParse ":wolfe.freenode.net 372 ck :- Thank you for using freenode!"
        `shouldBe` Right (Just (Server "wolfe.freenode.net"), ServerOnly "- Thank you for using freenode!")
  where
    doParse = parseOnly serverMessage

{-# LANGUAGE OverloadedStrings #-}
module IrcTypes
    ( IrcMessage(..)
    , Channel
    , Key
    , Source
    , joinNonEmpty
    , joinIrcChannels
    , sendText
    , readText
    , readNonEmpty
    ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text as A (Parser, char, endOfInput, many',
                                            maybeResult, parseOnly, string,
                                            take, takeText, takeWhile1)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as N
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T

-- type synonyms for text
type Channel = Text
type Key = Text
type Source = Text

-- | Represents the message formats sent across irc
data IrcMessage
    = Ping Text -- ^ A ping from the server, with text to be repeated
    | Pong Text -- ^ A pong sent from the client
    | Join (NonEmpty Channel) -- ^ Joining a list of channels, of which no strings may be empty
    | Part (NonEmpty Channel) -- ^ Leave a list of channels, of which no strings may be empty
    deriving (Eq, Show)

-- | Joins the elements of a nonempty list by sep
joinNonEmpty :: Text -> NonEmpty Text -> Text
joinNonEmpty sep = foldr1 (\x acc -> x <> sep <> acc)

-- | Joins a list of irc channels, as commonly formatted
joinIrcChannels :: NonEmpty Channel -> Text
joinIrcChannels = joinNonEmpty "," . fmap (T.cons '#')

-- | Formats an irc message ready for sending
-- `readText . sendText == Right`
sendText :: IrcMessage -> Text
sendText (Ping m)     = "PING :" <> m
sendText (Pong m)     = "PONG :" <> m
sendText (Join chans) = "JOIN :" <> joinIrcChannels chans
sendText (Part chans) = "PART :" <> joinIrcChannels chans

-- | Reads an irc message, without the source
readText :: Text -> Either String IrcMessage
readText = parseOnly (foldr1 (<|>) [readPing, readPong, readJoin, readPart])
  where
    wrapWith txt pars = string txt *> pars <* endOfInput
    readPing = Ping <$> wrapWith "PING :" takeText
    readPong = Pong <$> wrapWith "PONG :" takeText
    readJoin = Join <$> wrapWith "JOIN :" (readNonEmpty ',' '#')
    readPart = Part <$> wrapWith "PART :" (readNonEmpty ',' '#')

-- doesn't parse empty text between seperators
readNonEmpty :: Char -> Char -> Parser (NonEmpty Text)
readNonEmpty sep header =
    (:|)
    <$> parseItem
    <*> many' (A.take 1 *> parseItem) -- needed to get rid of the sep
  where
    parseItem = char header *> takeWhile1 (/= sep)


type ServerMessage = (IrcMessage, Source)

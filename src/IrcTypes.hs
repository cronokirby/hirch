{-# LANGUAGE OverloadedStrings #-}
module IrcTypes
    ( IrcMessage(..)
    , Source(..)
    , Channel
    , Key
    , ircMessage
    , joinNonEmpty
    , joinIrcChannels
    , parseServerMessage
    , readNonEmpty
    , sendText
    , serverMessage
    , source
    ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text as A (Parser, char, digit, endOfInput,
                                            inClass, many', maybeResult,
                                            parseOnly, skipWhile, string, take,
                                            takeText, takeWhile1)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as N
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T


-- type synonyms related to irc messages
type Channel = Text
type Key = Text

-- | Represents the message formats sent across irc
data IrcMessage
    = Ping Text -- ^ A ping from the server, with text to be repeated
    | Pong Text -- ^ A pong sent from the client
    | Join (NonEmpty Channel) -- ^ Joining a list of channels, of which no strings may be empty
    | Part (NonEmpty Channel) -- ^ Leave a list of channels, of which no strings may be empty
    | ServerOnly Text -- ^ can't be sent by the client
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

-- | Reads an irc message
ircMessage :: Parser IrcMessage
ircMessage = foldr1 (<|>) [readPing, readPong, readJoin, readPart, readServerOnly]
  where
    wrapWith txt pars = string txt *> pars
    readPing = Ping <$> wrapWith "PING :" takeText
    readPong = Pong <$> wrapWith "PONG :" takeText
    readJoin = Join <$> wrapWith "JOIN :" (readNonEmpty ',' '#')
    readPart = Part <$> wrapWith "PART :" (readNonEmpty ',' '#')
    readServerOnly = do
        digit
        skipWhile (/= ':')
        char ':'
        t <- takeText
        pure (ServerOnly t)



-- doesn't parse empty text between seperators
readNonEmpty :: Char -> Char -> Parser (NonEmpty Text)
readNonEmpty sep header =
    (:|)
    <$> parseItem
    <*> many' (A.take 1 *> parseItem) -- needed to get rid of the sep
  where
    parseItem = char header *> takeWhile1 (/= sep)


-- type synonyms used for sources
type Nick = Text
type UserName = Text
type Host = Text

-- | Represents a source for a message
data Source
    = Server Host
    | User Nick UserName Host
    deriving (Eq, Show)

type ServerMessage = (Maybe Source, IrcMessage)


-- | Reads the source of an irc message
source :: Parser Source
source = do
    t <- base
    user t <|> pure (Server t)
  where
    base = do
        char ':'
        takeWhile1 (not . inClass "! ")
    user nick = do
        char '!'
        name <- takeWhile1 (/= '@')
        A.take 1 -- skipping @
        host <- takeWhile1 (/= ' ')
        pure (User nick name host)


-- | Reads an entire server message, along with source
serverMessage :: Parser ServerMessage
serverMessage = (,) <$> (fmap Just (source <* char ' ') <|> pure Nothing) <*> ircMessage


-- | Parses a server message
parseServerMessage :: Text -> Maybe ServerMessage
parseServerMessage = eitherMaybe . parseOnly serverMessage
  where
    eitherMaybe (Right a) = Just a
    eitherMaybe (Left _)  = Nothing

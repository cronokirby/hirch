{-# LANGUAGE OverloadedStrings #-}
module IrcTypes where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T

-- type synonyms for text
type Channel = Text
type Key = Text
type Source = Text

-- | Represents the message formats sent across irc
data IrcMessage
    = Ping Text -- ^ A ping from the server, with text to be repeated
    | Pong Text -- ^ A pong sent from the client
    | Join (NonEmpty Channel) -- ^ Joining a list of channels
    | Part (NonEmpty Channel) -- ^ Leave a list of channels
    deriving (Show)

-- | Joins the elements of a nonempty list by sep
joinNonEmpty :: Text -> NonEmpty Text -> Text
joinNonEmpty sep = foldr1 (\x acc -> x <> sep <> acc)

-- | Joins a list of irc channels, as commonly formatted
joinIrcChannels :: NonEmpty Channel -> Text
joinIrcChannels = joinNonEmpty "," . fmap (T.cons '#')

-- | Formats an irc message ready for sending
sendText :: IrcMessage -> Text
sendText (Ping m)     = "PING :" <> m
sendText (Pong m)     = "PONG :" <> m
sendText (Join chans) = "JOIN :" <> joinIrcChannels chans



type ServerMessage = (IrcMessage, Source)

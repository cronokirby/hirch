{-# LANGUAGE OverloadedStrings #-}
module Hirch
    ( app
    ) where

import           Data.Monoid  ((<>))
import           Data.Text    (Text, pack)
import qualified Data.Text.IO as T
import           IrcTypes
import           Network      (PortID (..), PortNumber, connectTo)
import           System.IO    (BufferMode (..), Handle, hSetBuffering,
                               hSetEncoding, stdout, utf8)

server :: String
server = "irc.freenode.org"

port :: PortNumber
port = 6667

chan :: Text
chan = "#tutbot-testing"

nick :: Text
nick = "tutbot"

app :: IO ()
app = do
    hSetEncoding stdout utf8
    h <- connectTo server (PortNumber port)
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick <> " 0 * :tutorial bot")
    write h "JOIN" chan
    listen h

write :: Handle -> Text -> Text -> IO ()
write h s t = do
    T.hPutStr h written
    T.putStr ("> " <> written)
  where
    written = s <> " " <> t <> "\r\n"

listen :: Handle -> IO ()
listen h = do
    s <- T.hGetLine h
    T.putStrLn (pack . show . parseServerMessage $ s)
    listen h

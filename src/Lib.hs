module Lib
    ( someFunc
    ) where

import           Network     (PortID (..), PortNumber, connectTo)
import           System.IO   (BufferMode (..), Handle, hGetContents, hGetLine,
                              hSetBuffering)
import           Text.Printf (hPrintf, printf)


server :: String
server = "irc.freenode.org"

port :: PortNumber
port = 6667

chan :: String
chan = "#tutbot-testing"

nick :: String
nick = "tutbot"

someFunc :: IO ()
someFunc = do
    putStrLn "foo"
    h <- connectTo server (PortNumber port)
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick ++ " 0 * :tutorial bot")
    write h "JOIN" chan
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = do
    s <- hGetLine h
    putStrLn s
    listen h


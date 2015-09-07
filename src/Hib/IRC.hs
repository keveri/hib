module Hib.IRC
  ( privmsg
  , joinChannel
  , ping
  , pong
  , readMsg
  ) where

import Data.List (isPrefixOf)
import Data.Char (toLower)
import Hib.Types

-- PING check.
ping :: String -> Bool
ping = (isPrefixOf) "PING :"

-- PONG response.
pong :: String -> IRCLine
pong x = ("PONG",(':' : drop 6 x))

-- Form a privmsg to the current chan + server.
privmsg :: Channel -> String -> IRCLine
privmsg chan s = ("PRIVMSG",(chan ++ " :" ++ s))

-- Form instructions to join channel with nick.
joinChannel :: (Nick,Channel) -> [IRCLine]
joinChannel (nick,chan) =
    [ ("NICK",nick)
    , ("USER",(nick ++ " 0 * :bot"))
    , ("JOIN",chan)
    ]

-- Read IRC line to a Message.
readMsg :: String -> Message
readMsg s = Msg u ch c
  where u  = takeWhile (/= '!') $ drop 1 x
        ch = takeWhile (/= ' ') $ dropWhile (/= '#') x
        c  = drop 1 $ dropWhile (/= ':') $ drop 1 x
        x  = map toLower s

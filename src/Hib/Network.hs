module Hib.Network
  ( connect
  , write
  , writeLines
  ) where

import Control.Monad.Reader (asks, liftIO)
import Control.Exception
import Network
import System.IO
import Text.Printf
import Hib.Types

-- Send a message out to the server we're currently connected to
write :: IRCLine -> Net ()
write (s,t) = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

-- Send multiple lines
writeLines :: [IRCLine] -> Net ()
writeLines = mapM_ write

-- Connect to the server and return the initial bot state
connect :: HibConfig -> IO Bot
connect hc = notify $ do
    h <- connectTo server (PortNumber (fromIntegral (cPort hc)))
    hSetBuffering h NoBuffering
    return (Bot h hc)
  where
    server = cServer hc
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

module Hib.Core (startHib) where

import Control.Monad.Reader (asks, liftIO, runReaderT)
import Control.Exception
import System.IO
import Hib.Types
import Hib.Network
import Hib.Command
import Hib.IRC

-- Send privmsg to channel.
writePrivMsg :: Maybe String -> Net ()
writePrivMsg Nothing  = return ()
writePrivMsg (Just s) = do
    conf <- asks config
    write $ privmsg (cChannel conf) s

-- Message validations
validateMessage :: HibConfig -> Message -> Bool
validateMessage conf (Msg u ch _) =
    let self = (cNick conf) == u
        correctChan = (cChannel conf) == ch
    in  not self && correctChan

-- Evaluate command
evalCommand :: String -> Net ()
evalCommand s = do
    conf <- asks config
    let msg = readMsg s
    if validateMessage conf msg
      then writePrivMsg $ getCommand msg
      else return ()

-- Response PONG or evaluate command.
pingOrCommand :: String -> Net ()
pingOrCommand s
    | ping s    = write $ pong s
    | otherwise = evalCommand s

-- Process each line from the server.
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO (putStrLn s)
    pingOrCommand s
  where
    forever a = a >> forever a

-- Join a channel, and start processing commands.
run :: Net ()
run = do
    conf <- asks config
    let info = (cNick conf, cChannel conf)
    writeLines (joinChannel info) >> asks socket >>= listen

-- Start the bot.
startHib :: HibConfig -> IO ()
startHib hc = 
    let disconnect = hClose . socket
        loop st    = runReaderT run st
    in  bracket (connect hc) disconnect loop

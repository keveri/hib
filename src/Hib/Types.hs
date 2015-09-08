module Hib.Types where

import Control.Monad.Reader
import System.IO

data Message = Msg
  { msgUser    :: String
  , msgChannel :: String
  , msgContent :: String
  }

data Plugin = Plugin
  { pName :: String
  , pF    :: Message -> String
  }

data HibConfig = HibConfig
  { cServer  :: Server
  , cPort    :: Port
  , cNick    :: Nick
  , cChannel :: Channel
  }

data Bot = Bot
  { socket :: Handle
  , config :: HibConfig
  }

type Server = String
type Port = Int
type Nick = String
type Channel = String
type Net = ReaderT Bot IO
type Command = (String, (Message -> String))
type IRCLine = (String,String)

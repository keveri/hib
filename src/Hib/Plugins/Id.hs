module Hib.Plugins.Id (plugin) where

import Hib.Types

name :: String
name = "id"

f :: Message -> IO String
f (Msg _ _ c) = return (drop 4 c)

plugin :: Plugin
plugin = Plugin name f

module Hib.Plugins.Id (plugin) where

import Hib.Types

name :: String
name = "id"

f :: Message -> String
f (Msg _ _ c) = drop 4 c

plugin :: Plugin
plugin = Plugin name f

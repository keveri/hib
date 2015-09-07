{-# LANGUAGE OverloadedStrings #-}
module Hib.Config (readConfig) where

import qualified Data.Configurator as C
import Hib.Types

-- Read config file
readConfig :: FilePath -> IO HibConfig
readConfig cfgFile = do
  cfg     <- C.load [C.Required cfgFile]
  server  <- C.require cfg "server"
  port    <- C.require cfg "port"
  nick    <- C.require cfg "nick"
  channel <- C.require cfg "channel"
  return (HibConfig server port nick channel)

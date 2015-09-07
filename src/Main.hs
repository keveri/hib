module Main (main) where

import Hib.Core
import Hib.Config

-- Start hib with config file.
main :: IO ()
main = readConfig "hib.cfg" >>= startHib

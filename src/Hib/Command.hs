module Hib.Command (getCommand) where

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map       as M
import qualified Hib.Plugins.Id as Id
import           Hib.Types

-- List of plugins for bot.
plugins :: [Plugin]
plugins = [help, Id.plugin]

-- Help plugin for listing comamnds
help :: Plugin
help =
    let cmds = intercalate ", " $ map (\c -> '!':c) $ M.keys commands
        result = "Available commands: " ++ cmds
    in  Plugin "help" (\_ -> result)


-- Craete Plugin Map from list of Plugins.
pluginsToMap :: [Plugin] -> Map String (Message -> String)
pluginsToMap = M.fromList . map (\(Plugin n f) -> (n,f))

-- Map of commands.
commands :: Map String (Message -> String)
commands = pluginsToMap plugins

-- Return content from command if any.
getCommand :: Message -> Maybe String
getCommand msg@(Msg _ _ c) =
    let key = takeWhile (/= ' ') $ drop 1 c
        value = M.lookup key commands
    in  fmap (\cmd -> cmd msg) value

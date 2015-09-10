module Hib.Command (runCommand) where

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map          as M
import qualified Hib.Plugins.Id    as Id
import qualified Hib.Plugins.Piato as Piato
import           Hib.Types

-- List of plugins for bot.
plugins :: [Plugin]
plugins = [help, Id.plugin, Piato.plugin]

-- Help plugin for listing comamnds
help :: Plugin
help =
    let cmds = intercalate ", " $ map (\c -> '!':c) $ M.keys commands
        result = "Available commands: " ++ cmds
    in  Plugin "help" (\_ -> return (result))


-- Craete Plugin Map from list of Plugins.
pluginsToMap :: [Plugin] -> Map String (Message -> IO String)
pluginsToMap = M.fromList . map (\(Plugin n f) -> (n,f))

-- Map of commands.
commands :: Map String (Message -> IO String)
commands = pluginsToMap plugins

-- Return content from command if any.
runCommand :: Message -> IO (Maybe String)
runCommand msg@(Msg _ _ c) =
    let key = takeWhile (/= ' ') $ drop 1 c
    in  case M.lookup key commands of
             Nothing   -> return (Nothing)
             Just (fn) -> do
                x <- fn msg
                return (Just x)

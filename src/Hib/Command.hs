module Hib.Command (getCommand) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Hib.Types

-- Example command.
idMessage :: Message -> String
idMessage (Msg _ _ c) = drop 4 c

-- Map of commands.
commands :: Map String (Message -> String)
commands = M.fromList [("id" , idMessage)]

-- Return content from command if any.
getCommand :: Message -> Maybe String
getCommand msg@(Msg _ _ c) =
    let key = takeWhile (/= ' ') $ drop 1 c
        value = M.lookup key commands
    in  fmap (\cmd -> cmd msg) value

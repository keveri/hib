module Hib.Plugins.Piato (plugin) where

import Data.List (find)
import Data.Char (toLower)
import Hib.Types
import Text.XML.Light
import Network.HTTP (simpleHTTP, getResponseBody, getRequest)

type Restaurant = (String,String)

name :: String
name = "piato"

f :: Message -> IO String
f (Msg _ _ _) = parseMenu

plugin :: Plugin
plugin = Plugin name f

feed :: String
feed = "http://www.sonaatti.fi/rssfeed/"

-- clean restaurant name and menu.
cleanRestaurant :: (Maybe Element,Maybe Element) -> Maybe Restaurant
cleanRestaurant (Nothing,_)         = Nothing
cleanRestaurant (_,Nothing)         = Nothing
cleanRestaurant ((Just t),(Just d)) =
    let cleanT x = takeWhile (/= ' ') $ strContent x
        cleanD y = filter (not . flip  elem ['\t', '\n']) $ strContent y
    in  Just (cleanT t, cleanD d)

-- Parse restaurant from XML element.
parseRestaurant :: Element -> [Restaurant] -> [Restaurant]
parseRestaurant x acc =
    let mTitle = findElement (QName "title" Nothing Nothing) x
        mDesc = findElement (QName "description" Nothing Nothing) x
    in case cleanRestaurant (mTitle, mDesc) of
            Nothing     -> acc
            Just (food) -> food:acc

-- Parse restaurants from XML.
parseRestaurants :: Maybe Element -> [Restaurant]
parseRestaurants Nothing    = []
parseRestaurants (Just doc) =
    let itemElem = QName "item" Nothing Nothing
        items = findElements itemElem doc
    in  foldr parseRestaurant [] items

-- Restaurant as a string.
toStr :: Maybe Restaurant -> String
toStr Nothing = "Failed to parse."
toStr (Just (t,d)) = concat [t, ": ", d]

-- Find restaurant from list.
getRestaurant :: String -> [Restaurant] -> Maybe Restaurant
getRestaurant x = find (\(t,_) -> (map toLower t) == x)

-- Read RSS feed to string.
readFeed :: String -> IO String
readFeed url = simpleHTTP (getRequest url) >>= getResponseBody

-- Parse Sonaatti restaurants and return menu from Piato.
parseMenu :: IO String
parseMenu = do
    source <- readFeed feed
    let doc = parseXMLDoc source
        piato = getRestaurant "piato" (parseRestaurants doc)
    return (toStr piato)

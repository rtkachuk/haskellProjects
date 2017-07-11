{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module ServakDatabase where

import Data.Int
import Data.Char
import Data.List
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types
import System.IO
import System.Directory
import System.Exit

type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64

sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

-- Our row type. This type will carry data from database

data Item = Item { itemId :: Int,
                   itemTitle :: String, 
                   itemSection :: String, 
                   itemLink :: String, 
                   itemOwner :: String } deriving Show
                   
instance QueryResults Item where
  convertResults [fa, fb, fc, fd, fe] [va, vb, vc, vd, ve] = Item { itemId = a, itemTitle = b, itemSection = c, itemLink = d, itemOwner = e }
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
          e = convert fe ve
  convertResults fs vs = convertError fs vs 2

select :: Query -> SqlQuery [Item]
select qr = sqlQuery_ qr

fromInt64ToInt :: Int64 -> Int
fromInt64ToInt = fromIntegral

getSettingsFromFile :: String -> IO ConnectInfo
getSettingsFromFile path = do
  let settingsFile = path
  exists <- doesFileExist settingsFile
  
  if exists == True then do
  
    handle <- openFile settingsFile ReadMode
    isOpened <- hIsOpen handle
    hClose handle
    if | isOpened == False -> exitFailure
       | otherwise -> do
                      dat <- readFile settingsFile
                      let fromFile = lines dat
                      return ConnectInfo { connectHost = fromFile !! 0,
                                           connectPort = 3306,
                                           connectUser = fromFile !! 1,
                                           connectPassword = fromFile !! 2,
                                           connectDatabase = fromFile !! 3,
                                           connectOptions = [],
                                           connectPath = "",
                                           connectSSL = Nothing }
  else exitFailure

addRecord :: Item -> Connection -> IO Int64
addRecord (Item { itemId = id,
                 itemTitle = title, 
                 itemSection = section, 
                 itemLink = link, 
                 itemOwner = owner}) settings = do
  result <- execute settings "INSERT INTO items (title, section, link, owner) VALUES (?,?,?,?)" [title, section, link, owner]
  return result
  
getId :: Item -> Int
getId (Item { itemId = i,
                 itemTitle = t, 
                 itemSection = s, 
                 itemLink = l, 
                 itemOwner = o}) = i
  
searchRecord :: String -> Connection -> IO [Item]
searchRecord search settings = do
  result <- query settings "select `title`, `section`, `link`, `owner` from items WHERE `id` LIKE ? OR `title` LIKE ? OR `section` LIKE ? OR `link` LIKE ? OR `owner` LIKE ?" [search, search, search, search, search]
  return result
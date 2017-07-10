{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

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

-- Create some useful datatypes based on datatypes from module Database.MySQL

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

-- Create instance which will represent Item as QueryResult

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

-- Show item as normal people

showItem :: Item -> String
showItem (Item { itemId = i,
                 itemTitle = t, 
                 itemSection = s, 
                 itemLink = l, 
                 itemOwner = o}) = show (i) ++ " " ++ t ++ " " ++ s ++ " " ++ l ++ " " ++ o

-- This part of code will ask user to enter database connection
-- configuration data. It will be executed only in case of reject
-- reading from file

databaseConfigure :: IO ConnectInfo
databaseConfigure = do
  putStr "Enter address: "
  addr <- getLine
  putStr "Enter user: "
  user <- getLine
  putStr "Enter password: "
  password <- getLine
  putStr "Enter database: "
  dbName <- getLine
  return ConnectInfo { connectHost = addr,
                       connectPort = 3306,
                       connectUser = user,
                       connectPassword = password,
                       connectDatabase = dbName,
                       connectOptions = [],
                       connectPath = "",
                       connectSSL = Nothing }
                    
-- Here we will read data from file
                                            
getSettingsFromFile :: IO ConnectInfo
getSettingsFromFile = do
  let settingsFile = "settingsFile.txt"
  exists <- doesFileExist settingsFile
  
  if exists == True then do
  
    handle <- openFile settingsFile ReadMode
    isOpened <- hIsOpen handle
    hClose handle
    if | isOpened == False -> do
                              putStrLn ("Error opening File")
                              databaseConfigure
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
  else do
    putStrLn ("File not found")
    databaseConfigure

-- This part of code reading data from user input, creates row, and inserts it
-- to database
            
addRecord :: Connection -> IO()
addRecord settings = do
  putStr "Enter title: "
  title <- getLine
  putStr "Enter section: "
  section <- getLine
  putStr "Enter link: "
  link <- getLine
  putStr "Enter owner: "
  owner <- getLine
  result <- execute settings "INSERT INTO items (title, section, link, owner) VALUES (?,?,?,?)" [title, section, link, owner]
  putStrLn ((show $ result) ++ " affected")

-- Here we will search data by title

searchRecord :: Connection -> IO()
searchRecord settings = do
  putStrLn ("Enter search setting: ")
  search <- getLine
  result <- query settings "select `title`, `section`, `link`, `owner` from items WHERE `id` LIKE ? OR `title` LIKE ? OR `section` LIKE ? OR `link` LIKE ? OR `owner` LIKE ?" [search, search, search, search, search]
  let resultList = map (\n -> showItem n) result
  putStrLn "====================================\n"
  putStrLn $ intercalate "\n" resultList

-- Here we will drop record from database

dropRecord :: Connection -> IO()
dropRecord settings = do
  putStrLn ("Set id: ")
  mask <- getLine
  result <- execute settings "DELETE FROM items WHERE `id`=?" [mask]
  putStrLn ((show $ result) ++ " affected")

-- Here we will show all records

selectAllRecords :: Connection -> IO()
selectAllRecords conn = do
  putStrLn "\nData:"
  result <- query_ conn "select `id`, `title`, `section`, `link`, `owner` from items"
  let resultList = map (\n -> showItem n) result
  putStrLn "====================================\n"
  putStrLn $ intercalate "\n" resultList

-- This part of code kind of recursive function

selectionMenu :: Connection -> IO()
selectionMenu settings = do
  putStrLn ("\nDatabase connector: ")
  putStrLn "===================================="
  putStrLn ("Select action: ")
  putStrLn ("1 - Add record")
  putStrLn ("2 - Select all records")
  putStrLn ("3 - Search record")
  putStrLn ("4 - Drop record")
  putStrLn ("Anything else to exit")
  sel <- getLine
  case sel of
    "1" -> do
      addRecord settings
      selectionMenu settings
    "2" -> do
      selectAllRecords settings
      selectionMenu settings
    "3" -> do
      searchRecord settings
      selectionMenu settings
    "4" -> do
      dropRecord settings
      selectionMenu settings
    _ -> putStrLn ("End")

main = do
  hSetBuffering stdout NoBuffering
  putStr "Configure database connection from file?[Y/N]: "
  selection <- getLine
  if | selection == "Y" -> do
                           settings <- getSettingsFromFile
                           conn <- connect settings
                           selectionMenu conn
     | otherwise -> do
                    settings <- databaseConfigure
                    conn <- connect settings
                    selectionMenu conn
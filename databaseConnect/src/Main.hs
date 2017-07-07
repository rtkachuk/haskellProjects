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

type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64

sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

data Item = Item { itemTitle :: String, itemSection :: String, itemLink :: String, itemOwner :: String } deriving Show

instance QueryResults Item where
  convertResults [fa, fb, fc, fd] [va, vb, vc, vd] = Item { itemTitle = a, itemSection = b, itemLink = c, itemOwner = d }
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
  convertResults fs vs = convertError fs vs 2

select :: Query -> SqlQuery [Item]
select qr = sqlQuery_ qr

user :: (String, String, String, String) -> Item
user (newTitle, newSection, newLink, newOwner) = Item { itemTitle = newTitle, 
                                                        itemSection = newSection, 
                                                        itemLink = newLink, 
                                                        itemOwner = newOwner }

showItem :: Item -> String
showItem (Item {itemTitle = t, 
          itemSection = s, 
          itemLink = l, 
          itemOwner = o}) = t ++ " " ++ s ++ " " ++ l ++ " " ++ o

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
  result <- execute settings "INSERT INTO items VALUES (?,?,?,?)" [title, section, link, owner]
  putStrLn ((show $ result) ++ " affected")

searchRecord :: Connection -> IO()
searchRecord settings = do
  putStrLn ("Enter search setting: ")
  search <- getLine
  result <- query settings "select `title`, `section`, `link`, `owner` from items WHERE `title`=?" [search]
  let resultList = map (\n -> showItem n) result
  putStrLn "====================================\n"
  putStrLn $ intercalate "\n" resultList

dropRecord :: Connection -> IO()
dropRecord settings = do
  putStrLn ("Set mask: ")
  mask <- getLine
  result <- execute settings "DELETE FROM items WHERE `title`=?" [mask]
  putStrLn ((show $ result) ++ " affected")

selectAllRecords :: Connection -> IO()
selectAllRecords conn = do
  putStrLn "\nData:"
  result <- query_ conn "select `title`, `section`, `link`, `owner` from items"
  let resultList = map (\n -> showItem n) result
  putStrLn "====================================\n"
  putStrLn $ intercalate "\n" resultList

selectionMenu :: Connection -> IO()
selectionMenu settings = do
  putStrLn ("\nDatabase connector: ")
  putStrLn "===================================="
  putStrLn ("Select action: ")
  putStrLn ("1 - Add record")
  putStrLn ("2 - Select all records")
  putStrLn ("3 - Search record")
  putStrLn ("4 - Drop record")
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
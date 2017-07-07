{-# LANGUAGE OverloadedStrings #-}

import Data.Int
import Data.Char
import Data.List
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types

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
  putStrLn "Enter address: "
  addr <- getLine
  putStrLn "Enter user: "
  user <- getLine
  putStrLn "Enter password: "
  password <- getLine
  putStrLn "Enter database: "
  dbName <- getLine
  return ConnectInfo { connectHost = addr,
                            connectPort = 3306,
                            connectUser = user,
                            connectPassword = password,
                            connectDatabase = dbName,
                            connectOptions = [],
                            connectPath = "",
                            connectSSL = Nothing }

addRecord :: Connection -> IO()
addRecord settings = do
  putStrLn("Enter title: ")
  title <- getLine
  putStrLn("Enter section: ")
  section <- getLine
  putStrLn("Enter link: ")
  link <- getLine
  putStrLn("Enter owner: ")
  owner <- getLine
  result <- execute settings "INSERT INTO items VALUES (?,?,?,?)" [title, section, link, owner]
  putStrLn (show $ result)

searchRecord :: Connection -> IO()
searchRecord settings = do
  putStrLn ("Enter search setting: ")
  search <- getLine
  result <- query settings "select `title`, `section`, `link`, `owner` from items WHERE `title`=?" [search]
  let resultList = map (\n -> showItem n) result
  putStrLn "===================================="
  putStrLn $ intercalate "\n" resultList

selectAllRecords :: Connection -> IO()
selectAllRecords conn = do
  putStrLn "\nData:"
  result <- query_ conn "select `title`, `section`, `link`, `owner` from items"
  let resultList = map (\n -> showItem n) result
  putStrLn "===================================="
  putStrLn $ intercalate "\n" resultList

selectionMenu :: Connection -> IO()
selectionMenu settings = do
  putStrLn ("Database connector: ")
  putStrLn "===================================="
  putStrLn ("Select action: ")
  putStrLn ("1 - Add record")
  putStrLn ("2 - Select all records")
  putStrLn ("3 - Search record")
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
    _ -> putStrLn ("End")

main = do
  settings <- databaseConfigure
  conn <- connect settings
  selectionMenu conn
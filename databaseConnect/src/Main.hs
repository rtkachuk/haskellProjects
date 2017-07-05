{-# LANGUAGE OverloadedStrings #-}

-- Connect to localhost on port 3306 with user "adminUser", password "12341234", to database
-- "test". Database must contain "testing" table with two columns: id (Int) and name (Text)
--

import Data.Int
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.Types

type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64

sqlQuery :: (QueryParams q, QueryResults r) => Query -> q -> Connection -> IO [r]
sqlQuery q vs conn = query conn q vs

sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

sqlCmd :: QueryParams q => Query -> q -> Connection -> IO Int64
sqlCmd q vs conn = execute conn q vs

sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q

(>>>) :: SqlQuery a -> SqlQuery b -> SqlQuery b
(>>>) q1 q2 conn = do
  q1 conn
  q2 conn

connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "192.168.0.107",
                            connectPort = 6783,
                            connectUser = "site",
                            connectPassword = "12341234",
                            connectDatabase = "files",
                            connectOptions = [],
                            connectPath = "",
                            connectSSL = Nothing }

data Item = Item { itemTitle :: String, itemSection :: String, itemLink :: String, itemOwner :: String } deriving Show

instance QueryResults Item where
  convertResults [fa, fb, fc, fd] [va, vb, vc, vd] = Item { itemTitle = a, itemSection = b, itemLink = c, itemOwner = d }
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
  convertResults fs vs = convertError fs vs 2

select :: SqlQuery [Item]
select = sqlQuery_ "select `title`, `section`, `link`, `owner` from items"

user :: (String, String, String, String) -> Item
user (newTitle, newSection, newLink, newOwner) = Item { itemTitle = newTitle, itemSection = newSection, itemLink = newLink, itemOwner = newOwner }

main = do
  conn <- connect connectInfo
  result <- select conn
  putStrLn $ show result
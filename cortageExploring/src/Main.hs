{-# LANGUAGE MultiWayIf #-}

type Ip = String
type Port = Int
type Username = String
type Position = Int
type User = (Ip, Port, Username)

showComputerInfo :: User -> String
showComputerInfo (ip, port, user) =
  "Ip: " ++ ip ++ ", Port: " ++ show(port) ++ ", User: " ++ user

getName :: User -> Username
getName (_, _, name) = name

searchInfoByName :: [User] -> Username -> User
searchInfoByName [] name = ("", 0, "")
searchInfoByName database name = do
  let currentName = getName (head database) in
    if | currentName == name -> head database
       | otherwise -> searchInfoByName (tail database) name

main = do
  let database = [("78.154.90.34", 3308, "Kevin"),
                  ("194.39.15.07", 8080, "Jake"),
                  ("<???>", 0, "Guy, who know Haskell")]
  putStrLn("Let's see all members of our database: ")
  print(map (showComputerInfo) database)
  putStrLn("Ok, now, show the second user: ")
  print(showComputerInfo (database!!1))
  putStrLn("Good, now, let's find guy, who knows Haskell...")
  print (showComputerInfo (searchInfoByName database "Guy, who know Haskell"))
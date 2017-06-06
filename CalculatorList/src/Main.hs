-- This function will show all list values with indexes (1="+", 2="-", etc.)

showListAction :: [String] -> Int -> IO()
showListAction list (-1) = putStrLn(" ")
showListAction list number = do
  let index = ((length list) - number)-1
  putStr (show (index+1) ++ "=" ++ (list!!index) ++ " ")
  showListAction list (number-1)
  

main =
  let actionList = ["+", "-", "*", "/"] in
    do

-- Ask user for values

      putStrLn "Give me first value: "
      first <- getLine
      putStrLn "Give me first value: "
      second <- getLine

-- Show all actions from list, and ask him to select action

      putStr ("Select action: ")
      showListAction actionList ((length actionList)-1)
      action <- getLine

-- After action was read, calculate values

      case (read action :: Double) of
        1 -> putStrLn(show((read first :: Double) + (read second :: Double)))
        2 -> putStrLn(show((read first :: Double) - (read second :: Double)))
        3 -> putStrLn(show((read first :: Double) * (read second :: Double)))
        4 -> putStrLn(show((read first :: Double) / (read second :: Double)))
        _ -> putStrLn ("No such action")
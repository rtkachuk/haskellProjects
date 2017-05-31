recursiveUserInputProcess :: [Double] -> IO()
recursiveUserInputProcess listOfValues = do
  putStrLn ("Enter value: ")
  userInput <- getLine
  let valueToAdd = (read userInput :: Double)
  case valueToAdd of
    0 -> print $ product listOfValues
    _ -> do 
      let newList = valueToAdd : listOfValues;
      recursiveUserInputProcess newList

main = recursiveUserInputProcess []
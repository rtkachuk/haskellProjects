recursiveUserInputProcess :: [Double] -> IO()
recursiveUserInputProcess listOfValues = do
  putStrLn("Enter value: ")
  userInput <- getLine
  let valueToAdd = (read userInput :: Double)
  case valueToAdd of
    0 -> putStrLn(show(listOfValues))
    _ -> do valueToAdd : listOfValues;
                 recursiveUserInputProcess listOfValues

main = recursiveUserInputProcess [1]
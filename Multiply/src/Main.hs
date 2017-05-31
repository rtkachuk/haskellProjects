recursiveUserInputProcess :: [Double] -> IO()
recursiveUserInputProcess listOfValues = do
  putStrLn ("Enter value: ")
  userInput <- getLine
  let valueToAdd = (read userInput :: Double)
  case valueToAdd of
    0 -> showResult listOfValues
    _ -> recursiveUserInputProcess (valueToAdd `addValue` listOfValues)

addValue :: Double -> [Double] -> [Double]
addValue value valueList =
  value : valueList

showResult :: [Double] -> IO()
showResult listOfValues =
  print $ product listOfValues

main = recursiveUserInputProcess []
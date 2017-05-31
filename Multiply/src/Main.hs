getValue :: IO Double
getValue = do
  userInput <- getLine
  return (read userInput :: Double)

recursiveUserInputProcess :: [Double] -> IO()
recursiveUserInputProcess listOfValues = do
  putStrLn ("Enter value: ")
  valueToAdd <- getValue
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
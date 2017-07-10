import Data.Char

main = do
  putStrLn ("Foldr And Foldl exploring")
  let arr = [1..10]
  putStr ("Base array: ")
  print $ arr
  putStr ("Foldl with applied (-) to list: ")
  print $ foldl (-) 0 arr
  putStr ("Foldr with applied (-) to list: ")
  print $ foldr (-) 0 arr
generateTreeString :: Int -> Int -> String
generateTreeString starAmount spaceAmount =
  do
    if spaceAmount <= 0
      then 
        replicate spaceAmount ' ' ++ 
        replicate starAmount '*' ++ 
        "\n" ++ 
        replicate ((starAmount-1) `div` 2) ' ' ++ 
        "*\n"
    else 
      replicate spaceAmount ' ' ++ 
      replicate starAmount '*' ++ 
      "\n" ++ 
      generateTreeString (starAmount+2) (spaceAmount-1)

printTree :: Int -> String
printTree treeSize = 
  let 
    spacesAmountBeforeStars = ((treeSize-1) `div` 2)
  in
    generateTreeString 1 spacesAmountBeforeStars

main = do
  putStrLn("Tree size: ")
  treeSizeString <- getLine
  let treeSizeInt = (read treeSizeString :: Int)
  putStr(printTree (treeSizeInt))
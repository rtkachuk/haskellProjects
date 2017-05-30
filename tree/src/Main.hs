generateTreeString :: Int -> Int -> String
generateTreeString starAmount spaceAmount
  | spaceAmount <= 0 =
        replicate spaceAmount ' ' ++ 
        replicate starAmount '*' ++ 
        "\n" ++ 
        replicate ((starAmount-1) `div` 2) ' ' ++ 
        "*\n"
  | otherwise =
      replicate spaceAmount ' ' ++ 
      replicate starAmount '*' ++ 
      "\n" ++ 
      generateTreeString (starAmount+2) (spaceAmount-1)

printTree :: Int -> String
printTree treeSize = 
  let 
    spacesAmountBeforeStars = ((treeSize-1) `div` 2)
    starAmount = 1
  in
    generateTreeString starAmount spacesAmountBeforeStars

main = do
  putStrLn("Tree size: ")
  treeSizeString <- getLine
  let treeSizeInt = (read treeSizeString :: Int)
  putStr(printTree (treeSizeInt))
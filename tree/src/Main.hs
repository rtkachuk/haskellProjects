-- This function will generate row of the tree.
generateTreeString :: Int -> Int -> String
generateTreeString starAmount spaceAmount
-- If spacesAmount is 0 - stop function, and return our tree
  | spaceAmount <= 0 =
        replicate spaceAmount ' ' ++ 
        replicate starAmount '*' ++ 
        "\n" ++ 
-- This will generate tree end
        replicate ((starAmount-1) `div` 2) ' ' ++ 
        "*\n"
-- In other way, draw spaces and stars, then call it function
-- one more time, without one space
  | otherwise =
      replicate spaceAmount ' ' ++ 
      replicate starAmount '*' ++ 
      "\n" ++ 
      generateTreeString (starAmount+2) (spaceAmount-1)

-- As result, we will get whole tree

-- This function will generate string, which contain our tree
printTree :: Int -> String
printTree treeSize = 
  let 
-- Calculate spacesAmount
    spacesAmountBeforeStars = ((treeSize-1) `div` 2)
    starAmount = 1
  in
-- This function return one row of the tree.
    generateTreeString starAmount spacesAmountBeforeStars

main = do
  putStrLn("Tree size: ")
-- Get the tree size
  treeSizeString <- getLine
-- Transform entered data to Int
  let treeSizeInt = (read treeSizeString :: Int)
-- Now, call printTree function, and give it size as argument
  putStr(printTree (treeSizeInt))
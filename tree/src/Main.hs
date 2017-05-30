generateString :: Int -> Int -> String
generateString starAmount spaces =
  do
    if spaces <= 0
      then replicate spaces ' ' ++ replicate starAmount '*' ++ "\n" ++ replicate ((starAmount-1) `div` 2) ' ' ++ "*\n"
    else
      do replicate spaces ' ' ++ replicate starAmount '*' ++ "\n" ++ generateString (starAmount+2) (spaces-1)

printTree :: Int -> String
printTree size = generateString 1 ((size-1) `div` 2)

main = do
  putStrLn("Tree size: ")
  treeSizeString <- getLine
  let treeSizeInt = (read treeSizeString :: Int)
  putStr(printTree (treeSizeInt))
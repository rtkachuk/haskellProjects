printItems 0 mode= return()
printItems itemsAmount mode =
  do
    if mode == 0
      then do putStr(" ")
    else do putStr("*")
    printItems (itemsAmount-1) mode

printBranches 0 spaces stars = return()
printBranches branches spaces stars =
  do
    if branches < 0
      then return()
    else
      do
        printItems spaces 0
        printItems stars 1
        putStrLn("")
        printBranches (branches-2) (spaces-1) (stars+2)

printTree 0 = return()
printTree branches =
  do
    let spaces = (branches-1)/2
    printBranches branches spaces 1
    printItems spaces 0
    putStr("*")

main = do
  putStrLn("Enter number: ")
  treeHeightInString <- getLine
  let treeHeightInInt = (read treeHeightInString :: Int)
  if treeHeightInInt `mod` 2 == 0
    then putStrLn ("Error")
  else
    do
      let treeHeightInDouble = (read treeHeightInString :: Double)
      printTree (treeHeightInDouble)
      putStrLn("")
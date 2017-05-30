printSpace 0 = return()
printSpace n =
  do
    putStr(" ")
    printSpace (n-1)

printStars 0 = return()
printStars n =
  do
    putStr("*")
    printStars (n-1)

printBranches 0 a b = return()
printBranches n a b=
  do
    if n < 0
      then return()
    else
      do
        printSpace a
        printStars b
        putStrLn("")
        printBranches (n-2) (a-1) (b+2)

printTree 0 = return()
printTree n =
  do
    let spaces = (n-1)/2
    printBranches n spaces 1
    printSpace spaces
    putStr("*")

main = do
  putStrLn("Enter number: ")
  number <- getLine
  let x = (read number :: Int)
  if mod x 2 == 0
    then putStrLn ("Error")
  else
    do
      let value = (read number :: Double)
      printTree (value)
      putStrLn("")
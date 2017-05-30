calcAdd a b = a + b
calcSub a b = a - b
calcMul a b = a * b
calcDiv a b = a / b

main = do
  putStrLn("Give me first value: ")
  first <- getLine
  putStrLn("Give me second value: ")
  second <- getLine
  putStrLn("?Action: ")
  func <- getChar
  let x = (read first :: Double)
  let y = (read second :: Double)
  case func of
    '1' -> putStrLn(show(calcAdd x y))
    '2' -> putStrLn(show(calcSub x y))
    '3' -> putStrLn(show(calcMul x y))
    '4' -> putStrLn(show(calcDiv x y))
    _ -> putStrLn("Error: no such action")
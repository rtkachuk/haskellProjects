-- Functions without declaration

calcAdd a b = a + b
calcSub a b = a - b
calcMul a b = a * b
calcDiv a b = a / b

main = do

-- Here we will read action, first and second argument from user input

  putStrLn("Give me first value: ")
  first <- getLine
  putStrLn("Give me second value: ")
  second <- getLine
  putStrLn("?Action: ")
  func <- getChar

-- Transform it to double, and with if select action

  let x = (read first :: Double)
  let y = (read second :: Double)
  if func == '1'
    then putStrLn(show(calcAdd x y))
    else if func == '2'
      then putStrLn(show(calcSub x y))
      else if func == '3'
        then putStrLn(show(calcMul x y))
        else if func == '4'
          then putStrLn(show(calcDiv x y))
          else putStrLn("Error: no such action")
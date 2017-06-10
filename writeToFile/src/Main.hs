main = do
  let fileName = "/home/fabler/result.txt"

  writeFile fileName "Hello from Haskell\n"
  appendFile fileName $ "2+2 = " ++ show (2+2)
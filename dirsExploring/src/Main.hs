import System.Directory
import System.Console.ANSI

type DirName = String

showContents :: IO()
showContents = do
  curr <- getCurrentDirectory
  dt <- listDirectory curr
  putStrLn $ "Contents of: " ++ curr
  putStrLn "=========================="
  putStrLn $ unlines dt
  checkState 1 ""

changeDir :: DirName -> IO()
changeDir dir = do
  curr <- getCurrentDirectory
  let new = curr ++ "/" ++ dir
  exists <- doesPathExist new
  if exists == True
    then  do 
      setCurrentDirectory new
      checkState 1 ""
  else
    checkState 0 "Wrong path"

showError :: String -> IO()
showError err = do
  putStrLn err
  checkState 1 ""

waitInput :: IO()
waitInput = do
  putStrLn("cmd?>")
  cmd <- getLine
  clearScreen
  checkState 2 cmd

processCommand :: String -> IO()
processCommand cmd = do
  let act = take 2 cmd
  case act of
    "ls" -> checkState 4 ""
    "cd" -> checkState 3 dest where dest = drop 3 cmd
    "qu" -> checkState 5 ""
    otherwise -> checkState 0 "No such command"

exit :: IO()
exit = putStrLn ("Bye!")

checkState :: Int -> String -> IO()
checkState state cmd = do
  case state of 
    0 -> showError cmd
    1 -> waitInput
    2 -> processCommand cmd
    3 -> changeDir cmd
    4 -> showContents
    5 -> exit
    otherwise -> checkState 0 "Wrong action"

main = do
  setTitle "File manager"
  curr <- getHomeDirectory
  setCurrentDirectory curr
  checkState 1 ""
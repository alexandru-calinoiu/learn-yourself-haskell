import           Data.List                 (delete)
import           Distribution.Simple.Utils (withTempFile)
import           System.Directory          (removeFile, renameFile)
import           System.Environment        (getArgs)
import           System.Environment.Blank  (getProgName)
import           System.IO                 (hClose, hPutStr, openTempFile)

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch _        = error "Supported commands are: add, view and remove"

main :: IO ()
main = do
  (command:argList) <- getArgs
  dispatch command argList

view :: [String] -> IO ()
view [] = error "Must suply a filename"
view (fileName:_) = do
  contents <- readFile fileName
  let todoTasks = zipWith (\n line -> show n ++ ": " ++ line) [0..] $ lines contents
  mapM_ putStrLn todoTasks

remove :: [String] -> IO ()
remove []  = error "Must supply a filename"
remove [_] = error "Must supply a list of todos to remove"
remove (filename:todosToRemove)= do
  contents <- readFile filename
  let todoTasks = lines contents
      newTodos = unlines $ foldl (\list index -> delete (todoTasks !! read index) list) todoTasks todosToRemove

  withTempFile "." "temp" (\filePath handler -> do
    hPutStr handler newTodos
    removeFile "todo.txt"
    renameFile filePath "todo.txt")

add :: [String] -> IO ()
add [] = error "Must supply a filename"
add [_] = error "Must supply a list of todos to add"
add (filename:todos)= do
  mapM_ (\line -> appendFile filename $ line ++ "\n") todos


import           Control.Monad           (when)
import           CustomDataTypes         (Person (firstName))
import           Data.Char               (toUpper)
import           System.Console.Terminfo (Attributes (reverseAttr))

main :: IO ()
main = do
  input <- getLine
  when (input == "SWORDFISH") $ do
    putStrLn input

main''' = do
  line <- getLine
  if null line then do
    putStrLn "we are done!"
    return ()
  else do
    putStrLn $ reverseWords line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main'' = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how do you how how?"

main' = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", nice to meet your")

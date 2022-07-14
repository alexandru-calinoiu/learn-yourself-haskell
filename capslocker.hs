import           Control.Monad (forever)
import           Data.Char     (toUpper)

main :: IO ()
main = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome =
  unlines . map (\line -> if isPal line then "palindrome" else "is not palindrome") . lines

isPal :: String -> Bool
isPal str = str == reverse str

main''' = do
  content <- getContents
  putStrLn $ shortLinesOnly content

main'' = do
  content <- getContents
  putStrLn $ map toUpper content

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter isShort . lines
  where isShort l = length l < 10

main' = forever $ do
  l <- getLine
  putStrLn  $ map toUpper l

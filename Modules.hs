import           Data.Char (chr, digitToInt, isDigit, ord)
import           Data.List (any, find, group, isPrefixOf, nub, sort, tails,
                            words)
import           Data.Map  as Map (Map, fromList, fromListWith)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (isPrefixOf needle) . tails $ haystack

caesarEncode :: Int -> String -> String
caesarEncode = caesar

caesarDecode :: Int -> String -> String
caesarDecode offset = caesar (negate offset)

caesar :: Int -> String -> String
caesar offset = map (\x -> chr $ ord x + offset)

sumOfDigits :: (Integral a, Ord a) => a -> a
sumOfDigits n
  | n < 10 = n
  | otherwise = (n `mod` 10) + sumOfDigits (n `div` 10)

sumOfDigits' :: Int -> Int
sumOfDigits' = sum . map digitToInt . show

findOldLadyNumber :: Int -> Maybe Int
findOldLadyNumber n = find (\x -> sumOfDigits' x == n) [1..]

findByKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findByKey key xs = case find (\p -> fst p == key) xs of
                    Just result -> Just $ snd result
                    Nothing     -> Nothing

findByKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findByKey' _ [] = Nothing
findByKey' key ((k,v):xs)
  | key == k = Just v
  | otherwise = findByKey' key xs

findByKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findByKey'' key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

phoneBook :: Map.Map String String
phoneBook = Map.fromList
  [
  ("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("pasty", "493-2928")
  ]

convertPhoneNumber :: String -> [Int]
convertPhoneNumber = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
  where add n n' = n ++ ", " ++ n'

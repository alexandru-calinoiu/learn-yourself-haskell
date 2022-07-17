import           Data.Char (digitToInt, isNumber)

rpn :: String -> Int
rpn = head . foldl calculate [] . words

calculate :: [Int] -> String -> [Int]
calculate stack token
  | token == "+" = add stack
  | token == "-" = minus stack
  | token == "*" = multiply stack
  | otherwise = read token:stack

add :: [Int] -> [Int]
add []       = error"+ requires 2 terms none where found"
add [x]      = error"+ requires 2 terms only one was found"
add (x:y:xs) = x +y:xs

multiply :: [Int] -> [Int]
multiply []       = error"* requires 2 terms none where found"
multiply [x]      = error"* requires 2 terms only one was found"
multiply (x:y:xs) = x * y:xs

minus :: [Int] -> [Int]
minus []       = error"- requires 2 terms none where found"
minus [x]      = error"- requires 2 terms only one was found"
minus (x:y:xs) = y - x:xs

lucky :: Int -> String
lucky 7 = "LUCKY"
lucky x = "Sorry you are not very lucky"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' []    = error "No head for empty list dude"
head' (x:_) = x

firstLetter :: String -> String
firstLetter ""        = error "Empty string"
firstLetter all@(x:_) = "The first letter of: \"" ++ all ++ "\" is: " ++ show x

bmiTell :: Double -> String
bmiTell bm
  | bm <= 18.5 = "You are under, eat more"
  | bm <= 25.0 = "Looking good"
  | bm <= 30.0 ="Let's work out togheder"
  | otherwise = "Go see a doctor"

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

initials' :: String -> String -> String
initials' [] _        = error "No first name"
initials' _ []        = error "No last name"
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

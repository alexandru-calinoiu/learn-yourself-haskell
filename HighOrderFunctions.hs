isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

filter' :: Ord a => (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' g (x:xs)
  | g x = x : filter' g xs
  | otherwise = filter' g xs

sumWhile :: Int -> Int
sumWhile x = sum (takeWhile (<x) (filter odd (map (^2) [1..])))

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz x
  | even x = x : collatz (x `div` 2)
  | otherwise = x : collatz (x * 3 + 1)

howMany :: Int -> Int
howMany x = length (filter hasLenght (map collatz [1..100]))
  where hasLenght xs = length xs > x

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' :: (a -> b) -> [a] ->[b]
map' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length(takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))

fn :: (RealFrac a, Integral b, Floating a) => a -> b
fn = ceiling . negate . tan . cos . max 50

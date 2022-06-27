doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else doubleMe x

boomBang :: Integral a => [a] -> [[Char]]
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: Num a => [t] -> a
length' xs = sum[1 | _ <- xs]

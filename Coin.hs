import           System.Random (Random (randomRs), RandomGen, StdGen, getStdGen,
                                random)

main :: IO ()
main = do
  gen <- getStdGen
  putStrLn  $ take 20 (randomRs ('a', 'z') gen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, gen') = random gen
  in value:randoms' gen'

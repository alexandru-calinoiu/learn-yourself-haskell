import           Control.Monad (unless)
import           System.Random (Random (randomR), StdGen, getStdGen)

main :: IO ()
main = do
  gen <- getStdGen
  askForANumber gen

askForANumber :: StdGen -> IO ()
askForANumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "La ce numar intre 1 si 10 ma gandesc?"
  numberString <- getLine

  unless (null numberString) $ do
    checkNumber randNumber (reads numberString :: [(Int, String)])
    askForANumber newGen

checkNumber :: Int -> [(Int, String)] -> IO ()
checkNumber _ [] = putStrLn "Not a number"
checkNumber randNumber ((number, _):_)
  | randNumber == number = putStrLn "Ai ghicit!"
  | otherwise = putStrLn $ "Imi pare rau era " ++ show randNumber


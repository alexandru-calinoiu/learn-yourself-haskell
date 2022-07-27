import           Control.Applicative (Applicative (liftA2))
main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards"
  putStrLn $ "Yes, you said " ++ line ++ " backwards"

main' = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards"
  putStrLn $ "Yes, you said " ++ line' ++ " backwards"

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing          = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

myAction :: IO ()
myAction =  do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The 2 lines concatenated are: " ++ a

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

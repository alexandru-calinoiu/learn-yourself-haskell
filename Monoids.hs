newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a, b) } deriving (Show)

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y )

newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

lenghtCompare :: String -> String -> Ordering
lenghtCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

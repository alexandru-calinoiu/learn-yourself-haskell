module CustomDataTypes where

import qualified Data.Map                          as Map
import           System.Directory.Internal.Prelude (newEmptyMVar)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

data Bool' = True' | False'

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point
  deriving Show

data Person = Person { firstName    :: String
                      , lastName    :: String
                      , age         :: Int
                      , height      :: Float
                      , phoneNumber :: String
                      , flavor      :: String } deriving (Show, Eq, Read)

area :: Shape -> Float
area (Circle _ r)                            = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 -y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) x' y' = Circle (Point (x + x') (y + y')) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x y = Rectangle (Point (x1 + x1) (y1 + y)) (Point (x2 + x) (y2 + y))

data Vector a = Vector a a a deriving Show

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector i' j' k') = Vector (i + i') (j + j') (k + k')

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector i' j' k') = i * i' + j * j' + k * k'

vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vmult` (Vector i' j' k') = Vector (i * i') (j * j') (k * k')

data Day = Luni | Marti | Miercuri | Joi | Vineri | Sambata | Duminica deriving (Eq, Ord, Show, Read, Bounded, Enum)

--- Locker

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist"
  Just (state, code) -> if state /= Taken then Right code
                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken"

lockers :: LockerMap
lockers = Map.fromList
  [(100, (Taken, "ZD391"))
  , (101, (Free, "JAH31"))
  , (103, (Free, "IQSA9"))
  ]

--- Recursive Data Structures

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys      = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

--- Binary tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insertInTree :: (Ord a) => a -> Tree a -> Tree a
insertInTree x EmptyTree                     =  singleton x
insertInTree x (Node x' tl tr)
  | x > x' = Node x' tl (insertInTree x tr)
  | x < x' = Node x' (insertInTree x tl) tr
  | otherwise = Node x' tl tr

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node x' tl tr)
  | x > x' = treeElem x tr
  | x < x' = treeElem x tl
  | otherwise = True

--- Type classes

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red       = True
  Yellow == Yellow = True
  Green == Green   = True
  _ == _           = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
  fmap _ EmptyTree      = EmptyTree
  fmap f (Node x tr tl) = Node (f x) (fmap f tr) (fmap f tl)

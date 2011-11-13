{-# LANGUAGE Arrows, CPP #-}
module Baby where

-- BEGIN IMPORTS

import Control.Arrow
import Control.Applicative                              (Applicative, ZipList (..), (<$>), (<*>), (*>), (<*), liftA2, pure)
import Control.Exception                                (IOException (..), catch)
import Control.Monad                                    (MonadPlus, (<=<), ap, forM, forever, guard, join, liftM, liftM2, liftM3, liftM4, liftM5, mplus, mzero, when)
import Control.Monad.Error                              (strMsg)
import Control.Monad.Instances                          -- http://stackoverflow.com/questions/7687181
import Control.Monad.State                              (State (..), evalState, execState, get, put, runState, state)
import Control.Monad.Writer                             (Writer, runWriter, tell, writer)
import qualified Data.ByteString as ByteString          (pack, unpack)
import qualified Data.ByteString.Lazy as LazyByteString (cons, cons', empty, fromChunks, pack, readFile, unpack, writeFile)
import Data.Char                                        (chr, isDigit, ord, toLower, toUpper)
import Data.Foldable                                    (Foldable)
import qualified Data.Foldable as Foldable              (foldMap, foldl, foldl1, foldr, foldr1)
import Data.Function                                    (fix, on)
import Data.List                                        (find, genericLength, intercalate, intersperse, nub, sort, tails, transpose)
import Data.List.Utils                                  (split)
import Data.Map                                         (Map)
import qualified Data.Map as Map                        (fromList, lookup)
import Data.Monoid                                      (Monoid, All (..), Any (..), First (..), Last (..), Product (..), Sum (..), mappend, mconcat, mempty)
import Data.Tuple                                       (curry, fst, snd, swap, uncurry)
import System.Console.ANSI                              (clearScreen, setCursorPosition)
import System.Directory                                 (doesFileExist, renameFile)
import System.Environment                               (getArgs, getProgName)
import System.IO                                        (IOMode (ReadMode), hClose, hFlush, hGetContents, hPutStr, openFile, openTempFile, stdout, withFile)
import System.IO.Error                                  (isDoesNotExistError)
import System.Random                                    (StdGen, RandomGen, Random, getStdGen, mkStdGen, newStdGen, random, randomR, randomRIO, randomRs, randoms)
import Text.Printf                                      (PrintfArg, printf)

--- END IMPORTS

--- BEGIN UTILS

infixl 4 ?
c ? (t, e) = if c then t else e

decrement = subtract 1

--- END UTILS

doubleMe x = x + x

doubleUs' x y = doubleMe x + doubleMe y

doubleUs = curry $ doubleMe *** doubleMe >>> uncurry (+)

doubleSmallNumber' x = if x > 100 then x else x*2

doubleSmallNumber = (>100) &&& id &&& (*2) >>> uncurry (?)

conanO'Brien = "It's a-me, Conan O'Brien!"

removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

addThree' x y z = x + y + z

addThree = curry . curry $ uncurry (+) *** id >>> uncurry (+)

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' = (*) $ 2 * pi

lucky :: (Integral a) => a -> String
lucky 7 = "Luck number SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial' :: (Integral z) => z -> z
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

charName :: Char -> String
charName 'a' = "Alpha"
charName 'b' = "Bravo"
charName 'c' = "Charlie"
charName 'd' = "Delta"
charName 'e' = "Echo"
charName 'f' = "Foxtrot"
charName 'g' = "Golf"
charName 'h' = "Hotel"
charName 'i' = "India"
charName 'j' = "Juliet"
charName 'k' = "Kilo"
charName 'l' = "Lima"
charName 'm' = "Mike"
charName 'n' = "November"
charName 'o' = "Oscar"
charName 'p' = "Papa"
charName 'q' = "Quebec"
charName 'r' = "Romeo"
charName 's' = "Sierra"
charName 't' = "Tango"
charName 'u' = "Uniform"
charName 'v' = "Victor"
charName 'w' = "Whiskey"
charName 'x' = "X-ray"
charName 'y' = "Yankee"
charName 'z' = "Zulu"
charName '0' = "Zero"
charName '1' = "One"
charName '2' = "Two"
charName '3' = "Three"
charName '4' = "Four"
charName '5' = "Five"
charName '6' = "Six"
charName '7' = "Seven"
charName '8' = "Eight"
charName '9' = "Nine"
charName x = ""

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a, b) (c, d) = (a+c, b+d)

first' (a, _, _) = a
second' (_, b, _) = b
third' (_, _, c) = c

patternMatchInComprehensions x = [a+b | (a, b) <- x]

-- head' :: [a] -> a
-- head' [] = error "empty list"
-- head' (x:_) = x

class Nameable a where
  nameit :: a -> String

instance Nameable Char where
  nameit 'a' = "Alpha"
  nameit 'b' = "Bravo"
  nameit 'c' = "Charlie"
  nameit 'd' = "Delta"
  nameit 'e' = "Echo"
  nameit 'f' = "Foxtrot"
  nameit 'g' = "Golf"
  nameit 'h' = "Hotel"
  nameit 'i' = "India"
  nameit 'j' = "Juliet"
  nameit 'k' = "Kilo"
  nameit 'l' = "Lima"
  nameit 'm' = "Mike"
  nameit 'n' = "November"
  nameit 'o' = "Oscar"
  nameit 'p' = "Papa"
  nameit 'q' = "Quebec"
  nameit 'r' = "Romeo"
  nameit 's' = "Sierra"
  nameit 't' = "Tango"
  nameit 'u' = "Uniform"
  nameit 'v' = "Victor"
  nameit 'w' = "Whiskey"
  nameit 'x' = "X-ray"
  nameit 'y' = "Yankee"
  nameit 'z' = "Zulu"
  nameit _ = ""

instance Nameable Integer where
  nameit 0 = "Zero"
  nameit 1 = "One"
  nameit 2 = "Two"
  nameit 3 = "Three"
  nameit 4 = "Four"
  nameit 5 = "Five"
  nameit 6 = "Six"
  nameit 7 = "Seven"
  nameit 8 = "Eight"
  nameit 9 = "Nine"
  nameit _ = ""

listTell :: (Show a) => [a] -> String
listTell [] = "The list is empty"
listTell [x] = printf "The list has one element: [%s]" (show x)
listTell (x:y:[]) = printf "The list has two elements [%s, %s]" (show x) (show y)
listTell (x:y:_) = printf "This list is long.  The first two elemnts are: [%s, %s]" (show x) (show y)

length' :: (Num t) => [a] -> t
length' [] = 0
length' (_:xs) = 1 + length' xs

-- sum' :: (Num a) => [a] -> a
-- sum' [] = 0
-- sum' (a:b) = a + sum' b

sum' :: (Num a) => [a] ->  a
sum' = foldl1 (+)

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = printf "The first letter of: %s is %s" (show all) (show x)

bmiTell :: (Fractional t, Ord t) => t -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight fatty!"
  | otherwise = "You're a whale, congratulations!"

-- bmiTell' :: (RealFloat a) => a -> a -> a -> String
-- bmiTell' weight height_feet height_inches
--   | weight * 703 / (height_feet * 12 + height_inches) ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--   | weight * 703 / (height_feet * 12 + height_inches) ^ 2 <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
--   | weight * 703 / (height_feet * 12 + height_inches) ^ 2 <= 30.0 = "You're fat! Lose some weight fatty!"
--   | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a > b  = GT
  | a == b = EQ
  | a < b = LT

bmiTell' :: (RealFloat a) => a -> a -> a -> String
bmiTell' weight height_feet height_inches
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight * 703 / (height_feet * 12 + height_inches) ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials first last = printf "%c. %c." f l
  where (f, l) = (first !! 0, last !! 0)

{- list should be of type (w, hf, hi)
 - w - weight
 - hf - height in feet
 - hi - height in inches 
 -}
calcBmis :: (RealFloat a) => [(a, a, a)] -> [a]
calcBmis list = [bmi w hf hi | (w, hf, hi) <- list]
  where bmi w hf hi = w * 703 / (hf * 12 + hi) ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = 2 * pi * r^2
  in sideArea + topArea

{- list should be of type (w, hf, hi)
 - w - weight
 - hf - height in feet
 - hi - height in inches
 -}
calcBmis' :: (RealFloat a) => [(a, a, a)] -> [a]
calcBmis' list = [bmi | (w, hf, hi) <- list, let bmi = w * 703 / (hf * 12 + hi) ^ 2]

-- head' :: [a] -> a
-- head' a = case a of
--             [] -> error "empty list"
--             (x:_) -> x

-- describeList :: [a] -> String
-- describeList xs = "The list is " ++ case xs of
--                   [] -> "empty."
--                   _:[] -> "a singleton list."
--                   xs -> "a longer list."

-- describeList :: [a] -> String
-- describeList xs = "The list is " ++ a
--   where a = case xs of
--                 [] -> "empty."
--                 _:[] -> "a singleton list."
--                 xs -> "a longer list."


describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where what [] = "empty."
        what (_:[]) = "a singleton list."
        what (_:_) = "a longer list."

-- maximum' :: (Ord a) => [a] -> a
-- maximum' [] = error "empty list"
-- maximum' (head:[]) = head
-- maximum' (head:tail) = if head > maximum' tail then head else maximum' tail

-- maximum' :: (Ord a) => [a] -> a
-- maximum' [] = error "empty list"
-- maximum' (head:[]) = head
-- maximum' (head:tail)
--   | head > maxTail = head
--   | otherwise = maxTail
--   where maxTail = maximum' tail

-- maximum' :: (Ord a) => [a] -> a
-- maximum' [] = error "empty list"
-- maximum' (head:[]) = head
-- maximum' (head:tail) = max head $ maximum' tail

-- replicate' :: (Num a) => a -> b -> [b]
-- replicate' 0 _ = []
-- replicate' a b = b:replicate' (a - 1) b

replicate' :: (Num a) => a -> b -> [b]
replicate' 0 _ = []
replicate' a b = b : (r $ a - 1)
  where
    r = flip replicate' b

take' :: (Ord a, Num a) => a -> [b] -> [b]
take' _ [] = []
take' num _
  | num < 1 = []
take' num (head:rest) = head : take' (num - 1) rest

-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (head:rest) = (reverse' rest) ++ [head]

-- repeat' :: a -> [a]
-- repeat' a = a : repeat' a

repeat' :: a -> [a]
repeat' = fix . (:)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (ah:ar) (bh:br) = (ah, bh) : zip' ar br

-- elem' :: (Eq a) => a -> [a] -> Bool
-- elem' _ [] = False
-- elem' a (head:rest)
--   | a == head = True
--   | otherwise = elem' a rest

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\r i -> if r == True then r else i == x) False

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (head:rest) = (quicksort [r | r <- rest, r < head]) ++ [head] ++ (quicksort [r | r <- rest, r >= head])

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (head:rest) = (quicksort top) ++ [head] ++ (quicksort bottom)
  where
    (top, bottom) = pivot (< head) rest
    pivot :: (a -> Bool) -> [a] -> ([a], [a])
    pivot _pred []     = ([], [])
    pivot pred (x:xs)
      | pred x         = insertFst x $ pivot pred xs
      | otherwise      = insertSnd x $ pivot pred xs
    insertFst :: a -> ([a], [a]) -> ([a], [a])
    insertFst i (first, last) = (i:first, last)
    insertSnd :: a -> ([a], [a]) -> ([a], [a])
    insertSnd i (first, last) = (first, i:last)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort xl@(x:[]) = xl
mergesort list = merge $ conquer $ divide list
  where
    merge :: Ord a => ([a], [a]) -> [a]
    merge (l, []) = l
    merge ([], l) = l
    merge (xl@(x:xt), yl@(y:yt))
      | x < y = x:merge (xt, yl)
      | otherwise = y:merge (xl, yt)
    conquer :: Ord a => ([a], [a]) -> ([a], [a])
    conquer (top, bottom) = (mergesort top, mergesort bottom)
    divide :: [a] -> ([a], [a])
    divide [] = ([], [])
    divide (x:xs)
      | odd $ length xs = insertFst x $ divide xs
      | otherwise       = insertSnd x $ divide xs
    insertFst :: a -> ([a], [a]) -> ([a], [a])
    insertFst i (first, last) = (i:first, last)
    insertSnd :: a -> ([a], [a]) -> ([a], [a])
    insertSnd i (first, last) = (first, i:last)

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f a =  f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (xh:xt) (yh:yt) = f xh yh : zipWith' f xt yt

-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--   where g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

largestDivisible :: (Enum a, Num a, Integral a) => a -> a
largestDivisible max = head $ filter ((==) 0 . flip mod 3829) [max, max-1..]

chainQuery :: (Enum a, Integral a, Num b, Ord b, Num c) => a -> b -> c
chainQuery n min = fromIntegral . length . filter (> min) . map (fromIntegral . length . chain) $ [1..n]
  where
    chain :: (Integral a) => a -> [a]
    chain n
      | n < 1 = undefined
      | n == 1 = [n]
      | even n = n:(chain $ div n 2)
      | otherwise = n:(chain $ n * 3 + 1)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\i r -> f i:r) []
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\i r -> if i > r then i else r)
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
product' :: (Num a) => [a] -> a
product' = foldl1 (*)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\i r -> if f i then i:r else r) []
head' :: [a] -> a
head' = foldr1 (\i _r -> i)
last' :: [a] -> a
last' = foldl1 (\_r i -> i)

squareRootQuery :: (Ord a, Enum a, Num a, Floating a) => a -> a
squareRootQuery num = (+) 1 . fromIntegral . length . takeWhile (<num) . scanl1 (+) . map sqrt $ [1..]

ensureNegative :: (Num a) => [a] -> [a]
ensureNegative = map (negate . abs)

numUniques :: (Eq a, Num b) => [a] -> b
numUniques = genericLength . nub

encode :: Int -> [Char] -> [Char]
encode i = map (chr . (+i) . ord)

decode :: Int -> [Char] -> [Char]
decode = encode . negate

-- findKey :: (Ord k) => k -> [(k,v)] -> Maybe v
-- findKey first_name = fmap snd . DL.find (\(k,_) -> first_name == k)

-- findKey :: (Ord k) => k -> [(k,v)] -> Maybe v
-- findKey first_name = (snd <$>) . DL.find (\(k,_) -> first_name == k)

findKey :: (Ord k) => k -> [(k,v)] -> Maybe v
findKey first_name = Map.lookup first_name . Map.fromList

data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq)

surface :: Shape -> Float
surface (Circle _ radius) = pi * radius ^ 2
surface (Rectangle (Point t l) (Point b r)) = (abs $ b - t) * (abs $ r - l)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) x y = Circle (Point new_x new_y) r
  where
   new_x = x1+x
   new_y = y1+y
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x y = Rectangle (Point t l) (Point b r)
  where 
    t = x1+x
    l = y1+y
    b = x2+x
    r = y2+y

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person {firstName :: String, lastName :: String, age :: Int, height :: Float, phoneNumber :: String, flavor :: String} deriving (Show)

data Car a b c = Car {company :: a, model :: b, year :: c} deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company=company, model=model, year=year}) = printf "This %s %s was made in %s" company model (show year)

data Vector a = Vector a a a deriving (Show, Eq)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector ai aj ak) (Vector bi bj bk) = Vector (ai+bi) (aj+bj) (ak+bk)

vectMult :: (Num t) => Vector t -> t -> Vector t  
vectMult (Vector vi vj vk) x = Vector (vi*x) (vj*x) (vk*x)

scalarMult :: (Num t) => Vector t -> Vector t -> t  
scalarMult (Vector ai aj ak) (Vector bi bj bk) = ai*bi+aj*bj+ak*bk

type Dictionary x y = [(x, y)]
type ContactName = String
type PhoneNumber = String
type Phonebook = Dictionary ContactName PhoneNumber

phonebookLookup :: ContactName -> Phonebook -> Maybe PhoneNumber
phonebookLookup = findKey

inPhoneBook :: ContactName -> PhoneNumber -> Phonebook -> Bool
inPhoneBook contactname phonenumber phonebook = maybe False (const True) $ phonebookLookup contactname phonebook

data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap a = Map a (LockerState, Code)

lockerLookup :: (Ord a, PrintfArg a) => a -> LockerMap a -> Either String Code
lockerLookup lockerNumber lockers = results . Map.lookup lockerNumber $ lockers
  where
    results :: Maybe (LockerState, Code) -> Either String Code
    results Nothing = Left $ printf "Locker number %u doesn't exist!" lockerNumber
    results (Just (state, code))
      | state == Taken = Left $ printf "Locker %u is already taken!" lockerNumber
      | otherwise = Right code

lockers = Map.fromList [(100,(Taken,"ZD39I")), (101,(Free,"JAH3I")), (103,(Free,"IQSA9")), (105,(Free,"QOTSA")), (109,(Taken,"893JJ")), (110,(Taken,"99292"))]

infixr 5 :-:
data List a = EmptyList | a :-: (List a) deriving (Show, Read, Eq, Ord) 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

singleton :: a -> Tree a 
singleton a = Node a (EmptyTree) (EmptyTree)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node y left right)
  | x == y = tree
  | x < y = (Node y (treeInsert x left) right)
  | x > y = (Node y left (treeInsert x right))

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem a EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x < y = treeElem x left
  | x > y = treeElem x right

numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno (Node _ _ _) = True
  yesno EmptyTree = False

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf check yesResult noResult
  | yesno check == True = yesResult
  | otherwise = noResult

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

data Frank b a = Frank {frankField :: a b} deriving (Show)

data Barry t k p = Barry {yabba :: p, dabba :: t k} deriving (Eq, Show)

instance Functor (Barry t k) where
  fmap f (Barry p x) = Barry (f p) x

askName :: IO ()
askName = do  
  putStr "Hello, what's your name? "
  name <- getLine  
  let nameUpper = toUpper <$> name
  printf "Hey %s, you rock!\n" nameUpper

flipWords :: String -> String
flipWords = unwords . map reverse . words

reverseWords' ::  IO ()
reverseWords' = do
    putStr "Type a sentence: "
    s <- getLine
    if null s
        then do
          printf "Goodbye\n"
          return ()
        else do
          printf "In reverse it's: %s\n" $ flipWords s
          reverseWords

reverseWords ::  IO ()
reverseWords = do
    putStr "Type a sentence: "
    s <- getLine
    when (not $ null s) $ do
        printf "In reverse it's: %s\n" $ flipWords s
        reverseWords'

multipleReturns ::  IO ()
multipleReturns = do  
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putChar 'H'
    putChar 'A'
    putChar 'H'
    putChar 'A'
    putChar ' '
    putStrLn line

sequenceDemo' :: IO ()
sequenceDemo' = do  
    rs <- sequence [getLine, getLine, getLine]  
    sequence . map putStr $ ["You typed: "] ++ (intersperse " then " rs) ++ [".\n"]
    return ()

sequenceDemo :: IO ()
sequenceDemo = do  
    rs <- sequence [getLine, getLine, getLine]  
    mapM_ putStr $ ["You typed: "] ++ (intersperse " then " rs) ++ [".\n"]

foreverDemo :: IO ()
foreverDemo = forever $ putStr "Type a sentence: " >>= (\_ -> getLine) >>= printf "You wrote: %s\n"

forMDemo :: IO ()
forMDemo = do
    r <- forM [1,2,3,4] (\a -> do
        printf "How do you spell %u? " (a :: Int)
        getLine)
    printf "Your answers were: %s\n" . foldr1 (++) . intersperse ", " $ r

capsLocker :: IO ()
capsLocker = getContents >>= return . (map toUpper) >>= putStr

shortLinesOnly' :: IO ()
shortLinesOnly' = interact result
    where
        shortLength = 11
        onlyShorts  = (<= shortLength) . length
        shortLines  = filter onlyShorts . lines
        result      = unlines . shortLines
        interact    = (getContents >>=) . (putStr .)

shortLinesOnly :: IO ()
shortLinesOnly = interact result
    where
        shortLength = 11
        onlyShorts  = (<= shortLength) . length
        shortLines  = filter onlyShorts . lines
        result      = unlines . shortLines

data Discern a = Between a a

discern :: Discern a -> Bool -> a
discern (Between trueValue falseValue) test
    | test == True = trueValue
    | otherwise    = falseValue

respondPalindromes :: IO ()
respondPalindromes = interact result
    where
        reverseCheck x = x == reverse x
        isPalindrome   = discern (Between "palindrome" "not a palindrome") . reverseCheck
        checkLines     = fmap isPalindrome
        result         = unlines . checkLines . lines

girlfriend' :: IO ()
girlfriend' = readFile "girlfriend.txt" >>= putStr
    where
        readFile path = openFile path ReadMode >>= hGetContents

girlfriend'' :: IO ()
girlfriend'' = readFile "girlfriend.txt" >>= putStr

girlfriend''' :: IO ()
girlfriend''' = withFile "girlfriend.txt" ReadMode displayFromHandle
    where
        displayFromHandle handle = hGetContents handle >>= putStr

girlfriend :: IO ()
girlfriend = withFile "girlfriend.txt" ReadMode displayFromHandle
    where
        displayFromHandle handle = hGetContents handle >>= putStr
        withFile path mode hook = openFile path mode >>= hook

capGirlfriend :: IO ()
capGirlfriend = readFile "girlfriend.txt" >>= writeFile "girlfriend_cap.txt" . fmap toUpper

capendGirlfriend :: IO ()
capendGirlfriend = readFile "girlfriend.txt" >>= appendFile "girlfriend_cap.txt" . fmap toLower

-- todoDeleter :: IO ()
-- todoDeleter =
--     do
--         resetScreen
--         todoList <- getTodoList
--         display todoList
--         index <- promptForIndex
--         deleteAt index todoList >>= writeTodo
--     where
--         todoListPath   = "todo.txt"
--         indentation    = take 4 $ repeat ' '
--         printItem      :: Int -> String -> String
--         printItem      = printf "%u - %s"
--         prefixNumber   = zipWith printItem [1..]
--         indent         = fmap (indentation++)
--         resetScreen    = clearScreen >> setCursorPosition 0 0
--         getTodoList    = readFile todoListPath >>= return . lines
--         display        = (putStrLn "Here is your todo list:" >>) . putStr . unlines . indent . prefixNumber
--         promptForIndex = putStr "What item would you like to remove: " >> hFlush stdout >> getLine >>= return . (subtract 1) . read
--         deleteAt i l   = return (splitAt i l) >>= (\(a, b) -> return . concat $ [a,tail b]) 

todo :: IO ()
todo = 
    do
        rawArgs <- getArgs
        let
            (action:path:args) = rawArgs
        todoList <- getTodoList path
        newTodoList <- maybe (fail "Invalid Action") (\func -> func path todoList args) (lookup (action) routes)
        discern (return ()) (persist path newTodoList) (todoList == newTodoList)
    where
        discern trueValue falseValue test
            | test == True = trueValue
            | otherwise    = falseValue
        getTodoList path = do
            content <- doesFileExist path >>= discern (readFile path) (writeFile path "" >> return "")
            return . lines $ content
        persist path newTodoList = do
            (tempPath, fh) <- openTempFile "." path
            hPutStr fh $ unlines newTodoList
            hClose fh
            renameFile tempPath path
        view _ todoList _ = do
            mapM (putStrLn) $ zipWith (printf "%s - %s") (map (show) [1..]) todoList
            return todoList
        add path todoList additionItems = 
            return . concat $ [todoList, additionItems]
        remove path todoList (positionStr:_) = return newTodoList
            where
                position = read positionStr
                index = position - 1
                (firstPart, secondPart) = splitAt index todoList
                newTodoList = concat [firstPart, tail secondPart]
        replace path todoList (positionStr:newItem:_) = return newTodoList
            where
                position = read positionStr
                index = position - 1
                (firstPart, secondPart) = splitAt index todoList
                newTodoList = concat [firstPart, [newItem], tail secondPart]
        bump path todoList (positionStr:_) = return newTodoList
            where 
                position = read positionStr
                index = position - 1
                (firstPart, secondPartIncluding) = splitAt index todoList
                item = head secondPartIncluding
                secondPart = tail secondPartIncluding
                newTodoList = concat [[item], firstPart, secondPart]
        routes = [("add", add), ("view", view), ("remove", remove), ("replace", replace), ("bump", bump)]

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = (v1, v2, v3)
    where
        (v1, gen') = random gen
        (v2, gen'') = random gen'
        (v3, _) = random gen''

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = v:randoms' gen'
    where
        (v, gen') = random gen

finiteRandoms :: (RandomGen g, Random a, Num n) => g -> n -> [a]
finiteRandoms gen num
    | num == 0 = []
    | otherwise = v:finiteRandoms gen' (num-1)
        where
          (v, gen') = random gen

twoPasswords :: IO ()
twoPasswords = mapM_ (\a -> a >>= putStrLn) (take 2 passwords)
    where
        passwords = password:passwords
        password = newStdGen >>= return . take 6 . randomRs ('a', 'z')

guessOneToTen :: IO ()
guessOneToTen = 
    forever $ do
        number <- newNumber
        guessed <- firstPrompt
        untilM (== number) (const prompt) guessed
        correct
    where
        newNumber = randomRIO (1, 10) :: IO Int
        firstPrompt = putStr "Guess what number I am thinking of: " >> readLn
        prompt = putStr "Sorry try again: " >> readLn
        correct = putStrLn "You guessed correct!"
        untilM p f x
            | p x = return ()
            | otherwise = f x >>= untilM p f

performFileCopy :: IO ()
performFileCopy = Control.Exception.catch (perform) (handle)
    where
        perform = do
            (source:dest:_) <- getArgs
            fileCopy source dest
        handle :: IOException -> IO ()
        handle e
            | isDoesNotExistError e = print e
            | otherwise = ioError e
        fileCopy :: FilePath -> FilePath -> IO ()
        fileCopy source dest = LazyByteString.readFile source >>= LazyByteString.writeFile dest

rpnCalulator :: (Read a, Integral a, Num a) => String -> Either String a
rpnCalulator query = (foldl (rpnOperation) (Right []) . words $ query) >>= result
    where
        result stack@(r:remainingStack)
            | 0 == length remainingStack = Right r
            | otherwise = Left (printf "Incomplete expression with remaining numbers: %s." (show stack))
        rpnOperation (Right stack@(n1:n2:newStack)) op
            | op == "/" && n2 == 0 = Left "Divide by Zero"
            | op == "/" = Right (div n1 n2:newStack)
            | any (==op) ["x", "*"] = Right ((*) n1 n2:newStack)
            | op == "+" = Right ((+) n1 n2:newStack)
            | op == "-" = Right ((subtract) n1 n2:newStack)
            | not . all (isDigit) $ op = Left (printf "Invalid operator %s" (show op))
        rpnOperation (Right stack) v
            | not . all (isDigit) $ v = Left (printf "Not enough operends for %s: %s" (show v) (show stack))
            | otherwise = Right (read v:stack)
        rpnOperation r@(Left _) _ = r
performRpnCalulation :: IO ()
performRpnCalulation = getArgs >>= return . rpnCalulator . head >>= either (putStrLn) (putStrLn . show)

data Path a = NorthRoad a | SouthRoad a | CrossRoad a deriving (Show, Eq)
data Route a = Route {directions :: [Path a], distance :: a} deriving (Show, Eq)

instance (Ord a) => Ord (Route a) where
    compare (Route directions1 distance1) (Route directions2 distance2)
        | EQ == compare distance1 distance2  = (compare `on` length) directions1 directions2
        | otherwise                          = compare distance1 distance2

londonRoute = reconcileDirections . shortestDirections . foldl (shortestOnNorthAndSouth) (newRoute,newRoute) . foldr (groupByThree) [[]]
    where
        reconcileDirections (Route directions distance) = Route (reverse directions) distance
        shortestDirections = uncurry min
        appendToRoute (Route directions distance) newDirections = Route (newDirections ++ directions) sumDirections
            where
                sumDirections = foldl (sumPath) distance newDirections
                sumPath distance (NorthRoad newDistance) = distance + newDistance
                sumPath distance (SouthRoad newDistance) = distance + newDistance
                sumPath distance (CrossRoad newDistance) = distance + newDistance
        shortestOnNorthAndSouth (northRoute, southRoute) (northDistance:southDistance:crossDistance:[]) = (shortestNorthPoint, shortestSouthPoint)
            where
                northPointFromNorth = appendToRoute northRoute [NorthRoad northDistance]
                northPointFromSouth = appendToRoute southRoute [CrossRoad crossDistance, SouthRoad southDistance]
                southPointFromNorth = appendToRoute northRoute [CrossRoad crossDistance, NorthRoad northDistance]
                southPointFromSouth = appendToRoute southRoute [SouthRoad southDistance]
                shortestNorthPoint  = min northPointFromSouth northPointFromNorth
                shortestSouthPoint  = min southPointFromSouth southPointFromNorth
        newRoute = Route [] 0
        groupByThree :: String -> [[Int]] -> [[Int]]
        groupByThree rawV allThrees@(firstThree:restThrees)
            | length firstThree < 3 = (v:firstThree):restThrees 
            | otherwise = [v]:allThrees
            where
                v = read rawV
performLondonRoute = readFile "data/london.txt" >>= return . show . londonRoute . lines

performFmapExample = 
    do
        result <- fmap (intersperse '-' . reverse . map toUpper) $ getLine
        printf "You said %s backwards!\n" result
        printf "Yes you really said %s backwards!\n" result
        
data CMaybe a = CNothing | CJust Int a deriving (Show)

-- The CMaybe functor breaks the first functor law!
instance Functor CMaybe where
    fmap f (CJust c x) = CJust (c+1) (f x)
    fmap _ CNothing = CNothing

-- sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA' = foldr (\a b -> (:) <$> a <*> b) (pure [])

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

sequence' :: (Monad m) => [m a] -> m [a]
sequence' = foldr (liftM2 (:)) (return [])

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)} deriving (Eq, Show)

instance Functor (Pair b) where
    fmap f (Pair (a, b)) = Pair (f a, b)

lengthCompare :: String -> String -> Ordering
lengthCompare a b = mconcat . fmap (\f -> f a b) $ [(compare `on` length), (compare `on` vowels), compare]
    where
        vowels = length . filter (`elem` "aeiou")

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = Foldable.foldMap f l `mappend` f x `mappend` Foldable.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )

testTree' = Node 1
            (Node 3
                (Node 5 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe (Just a) f = f a
applyMaybe Nothing _ = Nothing

type Birds = Int
type Pole = (Birds, Birds)

emptyPole' :: Pole
emptyPole' = (0, 0)

landLeft' :: Birds -> Pole -> Pole
landLeft' c (l, r) = (l+c, r)

landRight' :: Birds -> Pole -> Pole
landRight' c (l, r) = (l, r+c)

(-:) :: a -> (a -> b) -> b
(-:) = flip ($)

emptyPole'' :: Maybe Pole
emptyPole'' = Just (0, 0)

landLeft'' :: Birds -> Pole -> Maybe Pole
landLeft'' c (l, r)
    | abs (l+c - r) < 4 = Just (l+c, r)
    | otherwise = Nothing

landRight'' :: Birds -> Pole -> Maybe Pole
landRight'' c (l, r)
    | abs (r+c - l) < 4 = Just (l, r+c)
    | otherwise = Nothing

banana'' :: Pole -> Maybe Pole
banana'' _ = Nothing

foo' :: Maybe String
foo' = Just 3 >>= (\x ->
       Just "!" >>= (\y ->
       Just ((show x) ++ y)))

foo :: Maybe String
foo =
    do
        x <- Just 3
        let y = "!";
            r = (show x) ++ y
        return r

marySue :: Maybe Bool
marySue =
    do
        x <- Just 9
        return (x > 8)

justH :: Maybe Char
justH =
    do
        x:xs <- Just "hello"
        return x

wopwop :: Maybe Char
wopwop =
    do
        x:xs <- Just ""
        return x

listOfTuples :: [(Int, Char)]
listOfTuples =
    do
        a <- [1, 2]
        b <- ['a', 'b']
        return (a, b)

sevensOnly :: [Int]
sevensOnly =
    do
        x <- [1..50]
        guard ('7' `elem` show x)
        return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (startX, startY) = sort result
    where
        inRange = and . sequence [(>0),(<9)]
        result =
            do
                deltaX <- (*) <$> [-1, 1] <*> [1, 2]
                deltaY <- (*) <$> [-1, 1] <*> [1, 2]
                guard (abs deltaX /= abs deltaY)
                let endX = startX + deltaX
                    endY = startY + deltaY
                guard (inRange endX && inRange endY)
                return (endX, endY)

in3 :: KnightPos -> [KnightPos]
in3 startPos = sort . nub $ (return startPos >>= moveKnight >>= moveKnight >>= moveKnight)

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 startPos endPos = endPos `elem` in3 startPos

isBigGang' :: (Ord a, Num a) => a -> Bool
isBigGang' = (>9)

isBigGang :: Int -> (Bool, String)
isBigGang = (>9) &&& printf "Compared gang size of %u to 9."

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b,m)
applyLog (n,oldLog) f = let (r,log) = f n in (r, oldLog `mappend` log)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int  
logNumber n = writer (n, [printf "Got number %u." $ n])

multWithLog :: Writer [String] Int
multWithLog = 
    do
        a <- logNumber 3
        b <- logNumber 5
        tell ["Gonna multiply these two"]
        return $ a * b

gcd' :: Int -> Int -> Writer [String] Int
gcd' x y
    | y == 0 = tell [printf "Finished with %u" x] >> return x
    | otherwise = tell [printf "%u mod %u = %u" x y remainder] >> gcd' y remainder
    where
        remainder = mod x y

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a 
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = toDiffList []
  mappend (DiffList f) (DiffList g) = DiffList (f . g)

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse x y
    | y == 0 = tell (toDiffList [printf "Finished with %u" x]) >> return x
    | otherwise = 
        do
           r <- gcdReverse y remainder
           tell . toDiffList $ [printf "%u mod %u = %u" x y remainder]
           return r
    where
        remainder = mod x y

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown n
    | n == 0 = tell . toDiffList $ ["0"]
    | otherwise = do
        r <- finalCountDown . decrement $ n
        tell . toDiffList . (:[]) . show $ n
        return r

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' n
    | n == 0 = tell ["0"]
    | otherwise = do
        r <- finalCountDown' . decrement $ n
        tell . (:[]) . show $ n
        return r

addStuff :: Int -> Int
addStuff =
    do
        a <- (*2)
        b <- (+10)
        return $ a + b

type Stack a = [a]

pop :: State (Stack a) a
pop = state $ head &&& tail

push :: a -> State (Stack a) ()
push x = state $ const () &&& (x:)

stackManip :: Num a => State (Stack a) a
stackManip = push 3 >> pop >> pop

stackStuff :: Num a => State (Stack a) ()
stackStuff = pop >>= \x -> if x == 5 then push 5 else push 3 >> push 8

moreStack :: Num a => State (Stack a) ()
moreStack = stackManip >>= \x -> if x == 100 then stackStuff else return ()

stackyStack :: Num a => State (Stack a) (Stack a)
stackyStack = get >>= (\s -> if s == [1,2,3] then put [8,3,1] else put [9,2,1]) >> get

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins' :: (RandomGen g) => State g (Bool,Bool,Bool)
threeCoins' = 
    do
        a <- randomSt
        b <- randomSt
        c <- randomSt
        return (a, b, c)

emptyPole :: Either String Pole
emptyPole = Right (0, 0)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (l, r)
    | abs (count - r) < 4 = Right (count, r)
    | otherwise = Left $ printf "Fell with %u birds perched on the left and %u birds perched on the right." count r
    where
        count = l + n

landRight :: Birds -> Pole -> Either String Pole
landRight n (l, r)
    | abs (count - l) < 4 = Right (l, count)
    | otherwise = Left $ printf "Fell with %u birds perched on the left and %u birds perched on the right." l count
    where
        count = r + n

banana :: Pole -> Either String Pole
banana _ = Left "Fell after slipping on a banana."

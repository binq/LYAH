module Baby where

import System.Console.ANSI (setCursorPosition, clearScreen)
import Text.Printf (PrintfArg (..), printf)
import Control.Monad (when, forever, forM, liftM)
import Control.Applicative (ZipList (..), (<$>), (<*>))
import Data.Function (fix)
import Data.Char (chr, ord, toUpper, toLower)
import Data.List (nub, intersperse, genericLength)
import System.IO (IOMode (..), stdout, openFile, withFile, hGetContents, hClose, openTempFile, hPutStr, hFlush)
import System.Directory (renameFile)
import Data.Map (Map (..))
import qualified Data.Map as Map (fromList, lookup)
import System.Environment (getArgs, getProgName)

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = if x > 100 then x else x*2 + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

addThree x y z = x + y + z

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

first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

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

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = printf "The list has one element: [%s]" (show x)
tell (x:y:[]) = printf "The list has two elements [%s, %s]" (show x) (show y)
tell (x:y:_) = printf "This list is long.  The first two elemnts are: [%s, %s]" (show x) (show y)

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

data Frank b a = Frank {frankField :: a b } deriving (Show)

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

todoDeleter :: IO ()
todoDeleter =
    do
        resetScreen
        todoList <- getTodoList
        display todoList
        index <- promptForIndex
        deleteAt index todoList >>= writeTodo
    where
        todoListPath   = "todo.txt"
        indentation    = take 4 $ repeat ' '
        printItem      :: Int -> String -> String
        printItem      = printf "%u - %s"
        prefixNumber   = zipWith printItem [1..]
        indent         = fmap (indentation++)
        resetScreen    = clearScreen >> setCursorPosition 0 0
        getTodoList    = readFile todoListPath >>= return . lines
        display        = (putStrLn "Here is your todo list:" >>) . putStr . unlines . indent . prefixNumber
        promptForIndex = putStr "What item would you like to remove: " >> hFlush stdout >> getLine >>= return . (subtract 1) . read
        deleteAt i l   = return (splitAt i l) >>= (\(a, b) -> return . concat $ [a,tail b]) 
        writeTodo l    = do
            (path, fh) <- openTempFile "." todoListPath
            hPutStr fh $ unlines l
            hClose fh
            renameFile path todoListPath
            putStrLn "Done"

todo :: IO ()
todo = getArgs >>= dispatch >>= putStrLn
    where
        add = show
        view = show
        remove = show
        replace = show
        dispatch (action:args) = return (maybe "" ($ args) (route action))
        routes = [("add", add), ("view", view), ("remove", remove), ("replace", replace)]
        route action = lookup action routes
        
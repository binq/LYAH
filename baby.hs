import Test.HUnit
import Text.Printf

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
circumference' r = 2 * pi * r

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

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

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

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (a:b) = a + sum' b

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = printf "The first letter of: %s is %s" (show all) (show x)

bmiTell :: (Fractional t, Ord t) => t -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> a -> String
bmiTell' weight height_feet height_inches
  | weight * 703 / (height_feet * 12 + height_inches) ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight * 703 / (height_feet * 12 + height_inches) ^ 2 <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
  | weight * 703 / (height_feet * 12 + height_inches) ^ 2 <= 30.0 = "You're fat! Lose some weight fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a > b  = GT
  | a == b = EQ
  | a < b = LT

bmiTell'' :: (RealFloat a) => a -> a -> a -> String
bmiTell'' weight height_feet height_inches
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

head'' :: [a] -> a
head'' a = case a of
           [] -> error "empty list"
           (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
                  [] -> "empty."
                  _:[] -> "a singleton list."
                  xs -> "a longer list."

describeList' xs = "The list is " ++ a
  where a = case xs of
                [] -> "empty."
                _:[] -> "a singleton list."
                xs -> "a longer list."

describeList'' xs = "The list is " ++ what xs
  where what [] = "empty."
        what (_:[]) = "a singleton list."
        what (_:_) = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' (head:[]) = head
maximum' (head:tail) = if head > maximum' tail then head else maximum' tail

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty list"
maximum'' (head:[]) = head
maximum'' (head:tail)
  | head > maxTail = head
  | otherwise = maxTail
  where maxTail = maximum'' tail

maximum''' :: (Ord a) => [a] -> a
maximum''' [] = error "empty list"
maximum''' (head:[]) = head
maximum''' (head:tail) = max head $ maximum''' tail

replicate' :: (Num a) => a -> b -> [b]
replicate' 0 _ = []
replicate' a b = b:replicate' (a - 1) b

replicate'' :: (Num a) => b -> a -> [b]
replicate'' _ 0 = []
replicate'' b a = 
  let replicate_b = replicate'' b
  in (:) b $ replicate_b $ a - 1

take' :: (Ord a, Num a) => a -> [b] -> [b]
take' _ [] = []
take' num _
  | num < 1 = []
take' num (head:rest) = head : take' (num - 1) rest

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (head:rest) = (reverse' rest) ++ [head]

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (ah:ar) (bh:br) = (ah, bh) : zip' ar br

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (head:rest)
  | a == head = True
  | otherwise = elem' a rest

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (head:rest) = (quicksort [r | r <- rest, r < head]) ++ [head] ++ (quicksort [r | r <- rest, r >= head])

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (head:rest) = (quicksort top) ++ [head] ++ (quicksort bottom)
  where
   (top, bottom) = pivot (< head) rest
   pivot :: (a -> Bool) -> [a] -> ([a], [a])
   pivot _pred []    = ([], [])
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

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

main = do
  runTestTT $ TestList [assertEqualTestCase 4 $ doubleMe 2,                                                                                 {- test 0 -}
                        assertEqualTestCase 10 $ doubleUs 2 3,                                                                              {- test 1 -}
                        assertEqualTestCase 4 $ doubleSmallNumber 2,                                                                        {- test 2 -}
                        assertEqualTestCase 1000 $ doubleSmallNumber' 1000,                                                                 {- test 3 -}
                        assertEqualTestCase "It's a-me, Conan O'Brien!" $ conanO'Brien,                                                     {- test 4 -}
                        assertEqualTestCase 6 $ addThree 1 2 3,                                                                             {- test 5 -}
                        assertEqualTestCase 24 $ factorial 4,                                                                               {- test 6 -}
                        assertEqualTestCase 25.1327412 $ circumference 4,                                                                   {- test 7 -}
                        assertEqualTestCase 25.132741228718345 $ circumference' 4,                                                          {- test 8 -}
                        assertEqualTestCase "Luck number SEVEN!" $ lucky 7,                                                                 {- test 9 -}
                        assertEqualTestCase 24 $ factorial' 4,                                                                              {- test 10 -}
                        assertEqualTestCase "Nine" $ charName '9',                                                                          {- test 11 -}
                        assertEqualTestCase (6, 10) $ addVectors (1, 3) (5, 7),                                                             {- test 12 -}
                        assertEqualTestCase 2 $ first (2, 4, 6),                                                                            {- test 13 -}
                        assertEqualTestCase 4 $ second (2, 4, 6),                                                                           {- test 14 -}
                        assertEqualTestCase 6 $ third (2, 4, 6),                                                                            {- test 15 -}
                        assertEqualTestCase [4, 7, 6, 8, 11, 4] $ patternMatchInComprehensions [(1,3), (4,3), (2,4), (5, 3), (5,6), (3,1)], {- test 16 -}
                        assertEqualTestCase 6 $ head' [6, 2, 3],                                                                            {- test 17 -}
                        assertEqualTestCase "Whiskey" $ nameit 'w',                                                                         {- test 18 -}
                        assertEqualTestCase "Nine" $ nameit (9 :: Integer),                                                                 {- test 19 -}
                        assertEqualTestCase "The list has one element: [1]" $ tell [1],                                                     {- test 20 -}
                        assertEqualTestCase 3 $ length' [1, 2, 3],                                                                          {- test 21 -}
                        assertEqualTestCase 10 $ sum' [1, 2, 3, 4],                                                                         {- test 22 -}
                        assertEqualTestCase "The first letter of: \"Hello World!\" is 'H'" $ capital "Hello World!",                        {- test 23 -}
                        assertEqualTestCase "You're fat! Lose some weight fatty!" $ bmiTell 27,                                             {- test 24 -}
                        assertEqualTestCase "You're fat! Lose some weight fatty!" $ bmiTell' 215 5 11,                                      {- test 25 -}
                        assertEqualTestCase 3 $ max' 2 3,                                                                                   {- test 26 -}
                        assertEqualTestCase EQ $ myCompare 3 3,                                                                             {- test 27 -}
                        assertEqualTestCase "You're fat! Lose some weight fatty!" $ bmiTell'' 215 5 11,                                     {- test 28 -}
                        assertEqualTestCase "V. S." $ initials "Vanson" "Samuel",                                                           {- test 29 -}
                        assertEqualTestCase [29.983138266217022] $ calcBmis [(215, 5, 11)],                                                 {- test 30 -}
                        assertEqualTestCase 87.96459430051421 $ cylinder 2 5,                                                               {- test 31 -}
                        assertEqualTestCase 42 $ 4 * (let x = 9 in x + 1) + 2,                                                              {- test 32 -}
                        assertEqualTestCase [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] $ [let square x = x^2 in square i | i <- [1..10]],       {- test 33 -}
                        assertEqualTestCase [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] $ [square | i <- [1..10], let square = i ^ 2],           {- test 34 -}
                        assertEqualTestCase [29.983138266217022] $ calcBmis' [(215, 5, 11)],                                                {- test 35 -}
                        assertEqualTestCase 100 $ head'' [100..200],                                                                        {- test 36 -}
                        assertEqualTestCase "The list is a longer list." $ describeList [1, 2],                                             {- test 37 -}
                        assertEqualTestCase "The list is a longer list." $ describeList' [1, 2],                                            {- test 38 -}
                        assertEqualTestCase "The list is a longer list." $ describeList'' [1, 2],                                           {- test 39 -}
                        {- Recursion -}
                        assertEqualTestCase 83 $ maximum' [45, 12, 11, 18, 27, 20, 82, 83, 26, 82],                                         {- test 40 -}
                        assertEqualTestCase 83 $ maximum'' [45, 12, 11, 18, 27, 20, 82, 83, 26, 82],                                        {- test 41 -}
                        assertEqualTestCase 83 $ maximum''' [45, 12, 11, 18, 27, 20, 82, 83, 26, 82],                                       {- test 42 -}
                        assertEqualTestCase "xxx" $ replicate' 3 'x',                                                                       {- test 43 -}
                        assertEqualTestCase "xxx" $ replicate'' 'x' 3,                                                                      {- test 44 -}
                        assertEqualTestCase [300, 301, 302] $ take' 3 [300..400],                                                           {- test 45 -}
                        assertEqualTestCase [5, 4, 3, 2, 1] $ reverse' [1..5],                                                              {- test 46 -}
                        assertEqualTestCase "xxx" $ take' 3 $ repeat 'x',                                                                   {- test 47 -}
                        assertEqualTestCase [('a', 1), ('b', 2), ('c', 3)] $ zip' "abc" [1, 2, 3],                                          {- test 48 -}
                        assertEqualTestCase True $ elem' 'x' "abxc",                                                                        {- test 49 -}
                        assertEqualTestCase [2, 6, 9, 11, 16, 29, 31, 56, 63, 96] $ quicksort [56, 11, 16, 9, 2, 96, 63, 31, 29, 6],        {- test 50 -}
                        assertEqualTestCase [2, 6, 9, 11, 16, 29, 31, 56, 63, 96] $ mergesort [56, 11, 16, 9, 2, 96, 63, 31, 29, 6],        {- test 51 -}
                        {- Higher order functions -}
                        assertEqualTestCase 2.0 $ divideByTen 20,                                                                           {- test 52 -}
                        assertEqualTestCase 8 $ applyTwice (*2) 2,                                                                          {- test 53 -}
                        assertEqualTestCase [10, 10, 10, 10, 10, 10, 10, 10, 10] $ zipWith' (+) [1..9] $ reverse [1..9],                    {- test 54 -}
                        assertEqualTestCase 2 $ flip' (/) 2 4,                                                                              {- test 55 -}
                        assertEqualTestCase 2 $ flip'' (/) 2 4,                                                                             {- test 56 -}
                        assertEqualTestCase [2, 6, 9, 11, 16, 29, 31, 56, 63, 96] $ quicksort' [56, 11, 16, 9, 2, 96, 63, 31, 29, 6],       {- test 57 -}
                        assertEqualTestCase True True]
  where
    assertEqualTestCase :: (Show a, Eq a) => a -> a -> Test
    assertEqualTestCase x y = TestCase $ assertEqual "" x y

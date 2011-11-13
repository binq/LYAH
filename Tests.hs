{-# LANGUAGE Arrows, CPP #-}

-- BEGIN IMPORTS
-- Loading Control.Monad.Instances to get access to (->) applicative instance: http://stackoverflow.com/questions/7687181

import Control.Arrow
import Control.Applicative                              (Applicative, ZipList (..), (<$>), (<*>), (*>), (<*), liftA2, pure)
import Control.Monad                                    (MonadPlus, (<=<), ap, forM, forever, guard, join, liftM, liftM2, liftM3, liftM4, liftM5, mplus, mzero, when)
import Control.Monad.Error                              (strMsg)
import Control.Monad.State                              (State (..), evalState, execState, get, put, runState, state)
import Control.Monad.Writer                             (Writer, runWriter, tell, writer)
import Data.Foldable                                    (Foldable)
import qualified Data.Foldable as Foldable              (foldMap, foldl, foldl1, foldr, foldr1)
import Data.List                                        (find, genericLength, intercalate, intersperse, nub, sort, tails, transpose)
import Data.Monoid                                      (Monoid, All (..), Any (..), First (..), Last (..), Product (..), Sum (..), mappend, mconcat, mempty)
import System.Random                                    (StdGen, RandomGen, Random, getStdGen, mkStdGen, newStdGen, random, randomR, randomRIO, randomRs, randoms)
import Text.Printf                                      (PrintfArg, printf)

import Baby 
import Test.HUnit.Base                                  (Test (TestCase, TestList), assertEqual)
import Test.HUnit.Text                                  (runTestTT)

--- END IMPORTS

main = do
  runTestTT $ TestList [assertEqualTestCase __LINE__ 4 $ doubleMe 2,
                        assertEqualTestCase __LINE__ 10 $ doubleUs 2 3,
                        assertEqualTestCase __LINE__ 4 $ doubleSmallNumber 2,
                        assertEqualTestCase __LINE__ 1000 $ doubleSmallNumber 1000,
                        assertEqualTestCase __LINE__ "It's a-me, Conan O'Brien!" $ conanO'Brien,
                        assertEqualTestCase __LINE__ 6 $ addThree 1 2 3,
                        assertEqualTestCase __LINE__ 24 $ factorial 4,
                        assertEqualTestCase __LINE__ 25.1327412 $ circumference 4,
                        assertEqualTestCase __LINE__ 25.132741228718345 $ circumference' 4,
                        assertEqualTestCase __LINE__ "Luck number SEVEN!" $ lucky 7,
                        assertEqualTestCase __LINE__ 24 $ factorial' 4,
                        assertEqualTestCase __LINE__ "Nine" $ charName '9',
                        assertEqualTestCase __LINE__ (6, 10) $ addVectors (1, 3) (5, 7),
                        assertEqualTestCase __LINE__ 2 $ first' (2, 4, 6),
                        assertEqualTestCase __LINE__ 4 $ second' (2, 4, 6),
                        assertEqualTestCase __LINE__ 6 $ third' (2, 4, 6),
                        assertEqualTestCase __LINE__ [4, 7, 6, 8, 11, 4] $ patternMatchInComprehensions [(1,3), (4,3), (2,4), (5, 3), (5,6), (3,1)],
                        assertEqualTestCase __LINE__ 6 $ head' [6, 2, 3],
                        assertEqualTestCase __LINE__ "Whiskey" $ nameit 'w',
                        assertEqualTestCase __LINE__ "Nine" $ nameit (9 :: Integer),
                        assertEqualTestCase __LINE__ "The list has one element: [1]" $ listTell [1],
                        assertEqualTestCase __LINE__ 3 $ length' [1, 2, 3],
                        assertEqualTestCase __LINE__ 10 $ sum' [1, 2, 3, 4],
                        assertEqualTestCase __LINE__ "The first letter of: \"Hello World!\" is 'H'" $ capital "Hello World!",
                        assertEqualTestCase __LINE__ "You're fat! Lose some weight fatty!" $ bmiTell 27,
                        assertEqualTestCase __LINE__ "You're fat! Lose some weight fatty!" $ bmiTell' 215 5 11,
                        assertEqualTestCase __LINE__ 3 $ max' 2 3,
                        assertEqualTestCase __LINE__ EQ $ myCompare 3 3,
                        assertEqualTestCase __LINE__ "V. S." $ initials "Vanson" "Samuel",
                        assertEqualTestCase __LINE__ [29.983138266217022] $ calcBmis [(215, 5, 11)],
                        assertEqualTestCase __LINE__ 87.96459430051421 $ cylinder 2 5,
                        assertEqualTestCase __LINE__ 42 $ 4 * (let x = 9 in x + 1) + 2,
                        assertEqualTestCase __LINE__ [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] $ [let square x = x^2 in square i | i <- [1..10]],
                        assertEqualTestCase __LINE__ [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] $ [square | i <- [1..10], let square = i ^ 2],
                        assertEqualTestCase __LINE__ [29.983138266217022] $ calcBmis' [(215, 5, 11)],
                        assertEqualTestCase __LINE__ "The list is a longer list." $ describeList [1, 2],
                        {- Recursion -}
                        assertEqualTestCase __LINE__ 83 $ maximum' [45, 12, 11, 18, 27, 20, 82, 83, 26, 82],
                        assertEqualTestCase __LINE__ "xxx" $ replicate' 3 'x',
                        assertEqualTestCase __LINE__ [300, 301, 302] $ take' 3 [300..400],
                        assertEqualTestCase __LINE__ [5, 4, 3, 2, 1] $ reverse' [1..5],
                        assertEqualTestCase __LINE__ "xxx" $ take' 3 $ repeat 'x',
                        assertEqualTestCase __LINE__ [('a', 1), ('b', 2), ('c', 3)] $ zip' "abc" [1, 2, 3],
                        assertEqualTestCase __LINE__ True $ elem' 'x' "abxc",
                        assertEqualTestCase __LINE__ [2, 6, 9, 11, 16, 29, 31, 56, 63, 96] $ quicksort [56, 11, 16, 9, 2, 96, 63, 31, 29, 6],
                        assertEqualTestCase __LINE__ [2, 6, 9, 11, 16, 29, 31, 56, 63, 96] $ mergesort [56, 11, 16, 9, 2, 96, 63, 31, 29, 6],
                        {- Higher order functions -}
                        assertEqualTestCase __LINE__ 2.0 $ divideByTen 20,
                        assertEqualTestCase __LINE__ 8 $ applyTwice (*2) 2,
                        assertEqualTestCase __LINE__ [10, 10, 10, 10, 10, 10, 10, 10, 10] $ zipWith' (+) [1..9] $ reverse [1..9],
                        assertEqualTestCase __LINE__ 2 $ flip' (/) 2 4,
                        assertEqualTestCase __LINE__ 99554 $ largestDivisible 100000,
                        assertEqualTestCase __LINE__ 66 $ chainQuery 100 15,
                        assertEqualTestCase __LINE__ [2, 3, 4] $ map' (\a -> a + 1) [1, 2, 3],
                        assertEqualTestCase __LINE__ [3, 2, 1] $ reverse' [1, 2, 3],
                        assertEqualTestCase __LINE__ 131 $ squareRootQuery 1000,
                        assertEqualTestCase __LINE__ [-1, -2, -3] $ ensureNegative [-1, 2, -3],
                        {- Modules -}
                        assertEqualTestCase __LINE__ 8 $ numUniques "hello world",
                        assertEqualTestCase __LINE__ "Khhhhh|" $ encode 3 "Heeeeey",
                        assertEqualTestCase __LINE__ "Heeeeey" $ decode 3 "Khhhhh|",
                        assertEqualTestCase __LINE__ (Just "452-2928") $ findKey "bonnie" [("betty","555-2938") ,("bonnie","452-2928") ,("patsy","493-2928") ,("lucille","205-2928") ,("wendy","939-8282") ,("penny","853-2492")],
                        {- Making Our Own Types and Typeclasses -}
                        assertEqualTestCase __LINE__ 314.15927 $ (surface $ Circle (Point 10 20) 10),
                        assertEqualTestCase __LINE__ 10000.0 $ (surface $ Rectangle (Point 0 0) (Point 100 100)),
                        assertEqualTestCase __LINE__ (Circle (Point 39.0 44.0) 10.0) $ nudge (Circle (Point 34 34) 10) 5 10,
                        assertEqualTestCase __LINE__ (Circle (Point 60.0 23.0) 5) $ nudge (baseCircle 5) 60 23,
                        assertEqualTestCase __LINE__ (Rectangle (Point 60.0 23.0) (Point 100.0 123.0)) $ nudge (baseRect 40 100) 60 23,
                        assertEqualTestCase __LINE__ "This Ford Mustang was made in 1967" $ (tellCar $ Car {company="Ford", model="Mustang", year=1967}),
                        assertEqualTestCase __LINE__ (Vector 12 7 16) $ vplus (Vector 3 5 8) (Vector 9 2 8),
                        assertEqualTestCase __LINE__ (Vector 12 9 19) $ vplus (Vector 3 5 8) $ vplus (Vector 9 2 8) (Vector 0 2 3),
                        assertEqualTestCase __LINE__ (Vector 30 90 70) $ vectMult (Vector 3 9 7) 10,
                        assertEqualTestCase __LINE__ (74.0) $ scalarMult (Vector 4 9 5) (Vector 9.0 2.0 4.0),
                        assertEqualTestCase __LINE__ (Vector 148 666 222) $ vectMult (Vector 2 9 3) $ scalarMult (Vector 4 9 5) (Vector 9 2 4),
                        assertEqualTestCase __LINE__ True $ inPhoneBook "bonnie" "452-2928" [("betty","555-2938"), ("bonnie","452-2928"), ("patsy","493-2928"), ("lucille","205-2928")],
                        assertEqualTestCase __LINE__ (Right "JAH3I") $ lockerLookup 101 Baby.lockers,
                        assertEqualTestCase __LINE__ (Left "Locker 100 is already taken!") $ lockerLookup 100 Baby.lockers,
                        assertEqualTestCase __LINE__ (Left "Locker number 102 doesn't exist!") $ lockerLookup 102 Baby.lockers,
                        assertEqualTestCase __LINE__ (Left "Locker 110 is already taken!") $ lockerLookup 110 Baby.lockers,
                        assertEqualTestCase __LINE__ (Right "QOTSA") $ lockerLookup 105 Baby.lockers,
                        assertEqualTestCase __LINE__ True $ 8 `treeElem` numsTree,
                        assertEqualTestCase __LINE__ False $ 100 `treeElem` numsTree,
                        assertEqualTestCase __LINE__ True $ 1 `treeElem` numsTree,
                        assertEqualTestCase __LINE__ False $ 10 `treeElem` numsTree,
                        assertEqualTestCase __LINE__ True $ Red == Red,
                        assertEqualTestCase __LINE__ False $ Red == Yellow,
                        assertEqualTestCase __LINE__ True $ Red `elem` [Red, Yellow, Green],
                        assertEqualTestCase __LINE__ "[Red light,Yellow light,Green light]" $ show $ [Red, Yellow, Green],
                        assertEqualTestCase __LINE__ False $ yesno $ length [],
                        assertEqualTestCase __LINE__ True $ yesno "haha",
                        assertEqualTestCase __LINE__ False $ yesno "",
                        assertEqualTestCase __LINE__ True $ yesno $ Just 0,
                        assertEqualTestCase __LINE__ True $ yesno True,
                        assertEqualTestCase __LINE__ False $ yesno EmptyTree,
                        assertEqualTestCase __LINE__ False $ yesno [],
                        assertEqualTestCase __LINE__ True $ yesno [0,0,0],
                        assertEqualTestCase __LINE__ "NO!" $ yesnoIf [] "YEAH!" "NO!",
                        assertEqualTestCase __LINE__ "YEAH!" $ yesnoIf [2,3,4] "YEAH!" "NO!",
                        assertEqualTestCase __LINE__ "YEAH!" $ yesnoIf True "YEAH!" "NO!",
                        assertEqualTestCase __LINE__ "YEAH!" $ yesnoIf (Just 500) "YEAH!" "NO!",
                        assertEqualTestCase __LINE__ "NO!" $ yesnoIf Nothing "YEAH!" "NO!",
                        assertEqualTestCase __LINE__ EmptyTree $ fmap (*2) EmptyTree,
                        assertEqualTestCase __LINE__ (Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree) $ fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7]),
                        assertEqualTestCase __LINE__ Barry {yabba = 4, dabba = Just 4} $ (+1) <$> Barry 3 (Just 4),
                        assertEqualTestCase __LINE__ (True,False,False) $ threeCoins . mkStdGen $ 100,
                        assertEqualTestCase __LINE__ [True,False,False,False,False] $ finiteRandoms (mkStdGen 100) 5,
                        assertEqualTestCase __LINE__ (Right 24) $ rpnCalulator "10 4 3 + 2 * +",
                        {- Functors, Applicative Functors and Monoids -}
                        assertEqualTestCase __LINE__ 303 $ fmap (*3) (+100) $ 1,
                        assertEqualTestCase __LINE__ 303 $ (*3) `fmap` (+100) $ 1,
                        assertEqualTestCase __LINE__ 303 $ (*3) . (+100) $ 1,
                        assertEqualTestCase __LINE__ "303" $ fmap (show . (*3)) (+100) $ 1,
                        assertEqualTestCase __LINE__ "303" $ show . (*3) . (+100) $ 1,
                        assertEqualTestCase __LINE__ [[1, 1, 1], [2, 2, 2], [3, 3, 3], [4, 4, 4]] $ fmap (replicate 3) [1, 2, 3, 4],
                        assertEqualTestCase __LINE__ (Just [4, 4, 4]) $ fmap (replicate 3) (Just 4),
                        assertEqualTestCase __LINE__ (Right ["blah", "blah", "blah"] :: Either String [String]) $ fmap (replicate 3) (Right "blah"),
                        assertEqualTestCase __LINE__ (Nothing :: Maybe [Int]) $ fmap (replicate 3) Nothing,
                        assertEqualTestCase __LINE__ (Left "foo" :: Either String [String]) $ fmap (replicate 3) (Left "foo"),
                        assertEqualTestCase __LINE__ (Just 3) $ fmap (id) (Just 3),
                        assertEqualTestCase __LINE__ (Just 3) $ id (Just 3),
                        assertEqualTestCase __LINE__ [1..5] $ fmap id [1..5],
                        assertEqualTestCase __LINE__ [1..5] $ id [1..5],
                        assertEqualTestCase __LINE__ ([] :: [String]) $ fmap id [],
                        assertEqualTestCase __LINE__ (Nothing :: Maybe String) $ fmap id Nothing,
                        assertEqualTestCase __LINE__ [50] $ fmap ((/2) . (*100)) [1],
                        assertEqualTestCase __LINE__ [50] $ fmap (/2) . fmap (*100) $ [1],
                        assertEqualTestCase __LINE__ [2, 4, 6, 8] $ fmap ($ 2) $ fmap (*) [1, 2, 3, 4],
                        assertEqualTestCase __LINE__ [2, 4, 6, 8] $ ($ 2) <$> (*) <$> [1, 2, 3, 4],
                        assertEqualTestCase __LINE__ (Just 12) $ Just (+3) <*> Just 9,
                        assertEqualTestCase __LINE__ (Just 13) $ pure (+3) <*> Just 10,
                        assertEqualTestCase __LINE__ (Just 12) $ (+3) <$> Just 9,
                        assertEqualTestCase __LINE__ Nothing $ pure (++"hahah") <*> Nothing,
                        assertEqualTestCase __LINE__ (Nothing :: Maybe String) $ Nothing <*> Just "woot",
                        assertEqualTestCase __LINE__ Nothing $ (Nothing :: Maybe (String -> String)) <*> Just "woot",
                        assertEqualTestCase __LINE__ (Just 8) $ pure (+) <*> Just 5 <*> Just 3,
                        assertEqualTestCase __LINE__ Nothing $ pure (+) <*> Just 3 <*> Nothing,
                        assertEqualTestCase __LINE__ Nothing $ pure (+) <*> Nothing <*> Just 5,
                        assertEqualTestCase __LINE__ (Just "johntravolta") $ (++) <$> Just "johntra" <*> Just "volta",
                        assertEqualTestCase __LINE__ "johntravolta" $ (++) "johntra" "volta",
                        assertEqualTestCase __LINE__ ["Hello"] $ pure "Hello",
                        assertEqualTestCase __LINE__ (Just "Hello") $ pure "Hello",
                        assertEqualTestCase __LINE__ [0, 0, 0, 101, 102, 103, 1, 4, 9] $ [(*0), (+100), (^2)] <*> [1, 2, 3],
                        assertEqualTestCase __LINE__ [4, 5, 5, 6, 3, 4, 6, 8] $ [(+), (*)] <*> [1, 2] <*> [3, 4],
                        assertEqualTestCase __LINE__ ["ha?", "ha!", "ha.", "heh?", "heh!", "heh.", "hmm?", "hmm!", "hmm."] $ (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."],
                        assertEqualTestCase __LINE__ [1, 2, 2, 3, 3, 4] $ [x + y | x <- [1, 2, 3], y <- [0, 1]],
                        assertEqualTestCase __LINE__ [1, 2, 2, 3, 3, 4] $ (+) <$> [1, 2, 3] <*> [0,1],
                        assertEqualTestCase __LINE__ [55, 80, 100, 110] $ filter (>50) $ (*) <$> [2, 5, 10] <*> [8, 10, 11],
                        assertEqualTestCase __LINE__ 3 $ (pure 3) "blah",
                        assertEqualTestCase __LINE__ 3 $ pure 3 "blah",
                        assertEqualTestCase __LINE__ 508 $ (+) <$> (*100) <*> (+3) $ 5,
                        assertEqualTestCase __LINE__ [7, 8.0, 2] $ (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 4,
                        assertEqualTestCase __LINE__ [101, 102, 103] $ getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList (replicate 3 100),
                        assertEqualTestCase __LINE__ [101, 102, 103] $ getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [100, 100..],
                        assertEqualTestCase __LINE__ [('d','c','r'),('o','a','a'),('g','t','t')] $ getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat",
                        assertEqualTestCase __LINE__ (Just [3,2,1]) $ sequenceA [Just 3, Just 2, Just 1],
                        assertEqualTestCase __LINE__ Nothing $ sequenceA [Just 3, Nothing, Just 1],
                        assertEqualTestCase __LINE__ [6, 5, 4] $ sequenceA [(+3),(+2),(+1)] 3,
                        assertEqualTestCase __LINE__ [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4], [3, 5], [3, 6]] $ sequenceA [[1,2,3],[4,5,6]],
                        assertEqualTestCase __LINE__ [] $ sequenceA [[1,2,3],[4,5,6],[3,4,4],[]],
                        assertEqualTestCase __LINE__ True $ and . sequenceA [(>4),(<10),odd] $ 7,
                        assertEqualTestCase __LINE__ [[1, 3], [1, 4], [2, 3], [2, 4]] $ sequenceA [[1, 2], [3, 4]],
                        assertEqualTestCase __LINE__ True $ CharList "benny" == CharList "benny",
                        assertEqualTestCase __LINE__ False $ CharList "benny" == CharList "oisters",
                        assertEqualTestCase __LINE__ (200, 3) $ getPair . fmap (*100) . Pair $ (2, 3),
                        assertEqualTestCase __LINE__ ("gnillac nodnol", 3) $ getPair . fmap reverse . Pair $ ("london calling", 3),
                        assertEqualTestCase __LINE__ [1, 2, 3, 4, 5, 6] $ [1, 2, 3] `mappend` [4, 5, 6],
                        assertEqualTestCase __LINE__ "onetwothree" $ ("one" `mappend` "two") `mappend` "three",
                        assertEqualTestCase __LINE__ "onetwothree" $ "one" `mappend` ("two" `mappend` "three"),
                        assertEqualTestCase __LINE__ "onetwothree" $ "one" `mappend` "two" `mappend` "three",
                        assertEqualTestCase __LINE__ "pang" $ "pang" `mappend` mempty,
                        assertEqualTestCase __LINE__ [1, 2, 3, 6, 9] $ mconcat [[1, 2], [3, 6], [9]],
                        assertEqualTestCase __LINE__ [] $ (mempty :: [Int]),
                        assertEqualTestCase __LINE__ 27 $ getProduct $ Product 3 `mappend` Product 9,
                        assertEqualTestCase __LINE__ 3 $ getProduct $ Product 3 `mappend` mempty,
                        assertEqualTestCase __LINE__ 24 $ getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2,
                        assertEqualTestCase __LINE__ 24 $ getProduct . mconcat . map Product $ [3, 4, 2],
                        assertEqualTestCase __LINE__ 11 $ getSum $ Sum 2 `mappend` Sum 9,
                        assertEqualTestCase __LINE__ 3 $ getSum $ mempty `mappend` Sum 3,
                        assertEqualTestCase __LINE__ 6 $ getSum . mconcat . map Sum $ [1, 2, 3],
                        assertEqualTestCase __LINE__ True $ getAny $ Any True `mappend` Any False,
                        assertEqualTestCase __LINE__ True $ getAny $ mempty `mappend` Any True,
                        assertEqualTestCase __LINE__ True $ getAny . mconcat . map Any $ [False, False, False, True],
                        assertEqualTestCase __LINE__ False $ getAny $ mempty `mappend` mempty,
                        assertEqualTestCase __LINE__ True $ getAll $ mempty `mappend` All True,
                        assertEqualTestCase __LINE__ False $ getAll $ mempty `mappend` All False,
                        assertEqualTestCase __LINE__ True $ getAll . mconcat . map All $ [True, True, True],
                        assertEqualTestCase __LINE__ False $ getAll . mconcat . map All $ [True, True, False],
                        assertEqualTestCase __LINE__ LT $ 1 `compare` 2,
                        assertEqualTestCase __LINE__ EQ $ 2 `compare` 2,
                        assertEqualTestCase __LINE__ GT $ 3 `compare` 2,
                        assertEqualTestCase __LINE__ LT $ LT `mappend` GT,
                        assertEqualTestCase __LINE__ GT $ GT `mappend` LT,
                        assertEqualTestCase __LINE__ LT $ mempty `mappend` LT,
                        assertEqualTestCase __LINE__ GT $ mempty `mappend` GT,
                        assertEqualTestCase __LINE__ LT $ lengthCompare "zen" "anna", 
                        assertEqualTestCase __LINE__ LT $ lengthCompare "zen" "ana",
                        assertEqualTestCase __LINE__ GT $ lengthCompare "zen" "ann",
                        assertEqualTestCase __LINE__ (Just "andy") $ Nothing `mappend` Just "andy",
                        assertEqualTestCase __LINE__ (Just LT) $ Just LT `mappend` Nothing,
                        assertEqualTestCase __LINE__ (Just 7) $ getSum <$> Just (Sum 3) `mappend` Just (Sum 4),
                        assertEqualTestCase __LINE__ (Just 'a') $ getFirst $ First (Just 'a') `mappend` First (Just 'b'),
                        assertEqualTestCase __LINE__ (Just 'b') $ getFirst $ First Nothing `mappend` First (Just 'b'),
                        assertEqualTestCase __LINE__ (Just 'a') $ getFirst $ First (Just 'a') `mappend` First Nothing,
                        assertEqualTestCase __LINE__ (Just 9) $ getFirst . mconcat . map First $ [Nothing, Just 9, Just 10],
                        assertEqualTestCase __LINE__ (Just 10) $ getLast . mconcat . map Last $ [Nothing, Just 9, Just 10],
                        assertEqualTestCase __LINE__ (Just "two") $ getLast $ Last (Just "one") `mappend` Last (Just "two"),
                        assertEqualTestCase __LINE__ 6 $ foldr (*) 1 [1,2,3],
                        assertEqualTestCase __LINE__ 6 $ Foldable.foldr (*) 1 [1,2,3],
                        assertEqualTestCase __LINE__ 11 $ Foldable.foldl (+) 2 (Just 9),
                        assertEqualTestCase __LINE__ True $ Foldable.foldr (||) False (Just True),
                        assertEqualTestCase __LINE__ 42 $ Foldable.foldl (+) 0 testTree,
                        assertEqualTestCase __LINE__ 64800 $ Foldable.foldl (*) 1 testTree,
                        assertEqualTestCase __LINE__ True $ getAny . Foldable.foldMap (Any . (<3)) $ testTree,
                        assertEqualTestCase __LINE__ False $ getAny . Foldable.foldMap (Any . (>15)) $ testTree,
                        assertEqualTestCase __LINE__ [1,3,6,5,8,9,10] $ Foldable.foldMap (:[]) testTree,
                        {- A Fistful of Monads -}
                        assertEqualTestCase __LINE__ (Just 4) $ Just 3 `applyMaybe` \x -> Just (x+1),
                        assertEqualTestCase __LINE__ (Just "smile :)") $ Just "smile" `applyMaybe` \x -> Just (x ++ " :)"),
                        assertEqualTestCase __LINE__ Nothing $ Nothing `applyMaybe` \x -> Just (x+1),
                        assertEqualTestCase __LINE__ Nothing $ Nothing `applyMaybe` \x -> Just (x ++ " :)"),
                        assertEqualTestCase __LINE__ (Just 3) $ Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing,
                        assertEqualTestCase __LINE__ Nothing $ Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing,
                        assertEqualTestCase __LINE__ (Just "WHAT") $ (return "WHAT" :: Maybe String),
                        assertEqualTestCase __LINE__ (Just 90) $ Just 9 >>= return . (*10),
                        assertEqualTestCase __LINE__ Nothing $ Nothing >>= return . (*10),
                        assertEqualTestCase __LINE__ 300 $ 100 -: (*3),
                        assertEqualTestCase __LINE__ False $ True -: not,
                        assertEqualTestCase __LINE__ (2, 0) $ emptyPole' -: landLeft' 2,
                        assertEqualTestCase __LINE__ (3, 1) $ emptyPole' -: landLeft' 1 -: landRight' 1 -: landLeft' 2,
                        assertEqualTestCase __LINE__ (10, 3) $ emptyPole' -: landRight' 3 -: landLeft' 10,
                        assertEqualTestCase __LINE__ (0, 2) $ emptyPole' -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2),
                        assertEqualTestCase __LINE__ (Just (2,0)) $ landLeft'' 2 (0,0),
                        assertEqualTestCase __LINE__ Nothing $ landLeft'' 10 (0,3),
                        assertEqualTestCase __LINE__ (Just (2,1)) $ landRight'' 1 (0,0) >>= landLeft'' 2,
                        assertEqualTestCase __LINE__ Nothing $ Nothing >>= landLeft'' 2,
                        assertEqualTestCase __LINE__ (Just (2,4)) $ return (0,0) >>= landRight'' 2 >>= landLeft'' 2 >>= landRight'' 2,
                        assertEqualTestCase __LINE__ Nothing $ return (0,0) >>= landLeft'' 1 >>= landRight'' 4 >>= landLeft'' (-1) >>= landRight'' (-2),
                        assertEqualTestCase __LINE__ Nothing $ return (0,0) >>= landLeft'' 1 >>= banana'' >>= landRight'' 1,
                        assertEqualTestCase __LINE__ Nothing $ return (0,0) >>= landLeft'' 1 >> Nothing >>= landRight'' 1,
                        assertEqualTestCase __LINE__ Nothing $ Nothing >> Just 3,
                        assertEqualTestCase __LINE__ (Just 4) $ Just 3 >> Just 4,
                        assertEqualTestCase __LINE__ (Nothing :: Maybe Int) $ Just 3 >> Nothing,
                        assertEqualTestCase __LINE__ (Just "3!") $ Just 3 >>= (\x -> Just (show x ++ "!")),
                        assertEqualTestCase __LINE__ (Just "3!") $ Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))),
                        assertEqualTestCase __LINE__ Nothing $ (Nothing :: Maybe Int) >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))),
                        assertEqualTestCase __LINE__ Nothing $ Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y))),
                        assertEqualTestCase __LINE__ Nothing $ Just 3 >>= (\x -> Just "!" >>= (\y -> (Nothing :: Maybe String))),
                        assertEqualTestCase __LINE__ (Just "3!") $ foo',
                        assertEqualTestCase __LINE__ (Just "3!") $ foo,
                        assertEqualTestCase __LINE__ (Just True) $ Just 9 >>= return.(>8),
                        assertEqualTestCase __LINE__ (Just True) $ marySue,
                        assertEqualTestCase __LINE__ (Just 'h') $ justH,
                        assertEqualTestCase __LINE__ Nothing $ wopwop,
                        assertEqualTestCase __LINE__ [3,-3,4,-4,5,-5] $ [3,4,5] >>= \x -> [x,-x],
                        assertEqualTestCase __LINE__ [] $ [] >>= \x -> ["bad","mad","rad"],
                        assertEqualTestCase __LINE__ ([] :: [Int]) $ [1,2,3] >>= \x -> [],
                        assertEqualTestCase __LINE__ [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')] $ [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch),
                        assertEqualTestCase __LINE__ [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')] $ listOfTuples,
                        assertEqualTestCase __LINE__ [7, 17, 27, 37, 47] $ [1..50] >>= (\x -> guard ('7' `elem` show x) >> [x]),
                        assertEqualTestCase __LINE__ ["cool"] $ guard (5 > 2) >> (return "cool" :: [String]),
                        assertEqualTestCase __LINE__ [] $ guard (1 > 2) >> (return "cool" :: [String]),
                        assertEqualTestCase __LINE__ [7, 17, 27, 37, 47] $ sevensOnly,
                        assertEqualTestCase __LINE__ (sort [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]) $ moveKnight (6,2),
                        assertEqualTestCase __LINE__ (sort [(6,2),(7,3)]) $ moveKnight (8,1),
                        assertEqualTestCase __LINE__ True $ (6,2) `canReachIn3` (6,1),
                        assertEqualTestCase __LINE__ False $ (6,2) `canReachIn3` (7,3),
                        assertEqualTestCase __LINE__ [9,-9,6,-6] $ sequence [id, negate] <=< sequence [(*3), (*2)] $ 3,
                        {- For a Few Monads More -}
                        assertEqualTestCase __LINE__ (False, "Compared gang size of 3 to 9.") $ isBigGang 3,
                        assertEqualTestCase __LINE__ (True, "Compared gang size of 30 to 9.") $ isBigGang 30,
                        assertEqualTestCase __LINE__ (False, "Smallish gang.Compared gang size of 3 to 9.") $ (3, "Smallish gang.") `applyLog` isBigGang,
                        assertEqualTestCase __LINE__ (True, "A freaking platoon.Compared gang size of 30 to 9.") $ (30, "A freaking platoon.") `applyLog` isBigGang,
                        assertEqualTestCase __LINE__ (5, "Got outlaw name.Applied length.") $ ("Tobin", "Got outlaw name.") `applyLog` (length &&& const "Applied length."),
                        assertEqualTestCase __LINE__ (7, "Got outlaw name.Applied length.") $ ("Bathcat","Got outlaw name.") `applyLog` (length &&& const "Applied length."),
                        assertEqualTestCase __LINE__ ("milk",Sum 35) $ ("beans", Sum 10) `applyLog` addDrink,
                        assertEqualTestCase __LINE__ ("whiskey",Sum 124) $ ("jerky", Sum 25) `applyLog` addDrink,
                        assertEqualTestCase __LINE__ ("beer",Sum 35) $ ("dogmeat", Sum 5) `applyLog` addDrink,
                        assertEqualTestCase __LINE__ ("beer",Sum 65) $ ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink,
                        assertEqualTestCase __LINE__ (3, "") $ runWriter (return 3 :: Writer String Int),
                        assertEqualTestCase __LINE__ (3, Sum 0) $ runWriter (return 3 :: Writer (Sum Int) Int),
                        assertEqualTestCase __LINE__ (3, Product 1) $ runWriter (return 3 :: Writer (Product Int) Int),
                        assertEqualTestCase __LINE__ (15, ["Got number 3.", "Got number 5.", "Gonna multiply these two"]) $ runWriter multWithLog,
                        assertEqualTestCase __LINE__ (1, ["8 mod 3 = 2","3 mod 2 = 1", "2 mod 1 = 0", "Finished with 1"]) $ runWriter $ gcd' 8 3,
                        assertEqualTestCase __LINE__ (3, ["9 mod 24 = 9","24 mod 9 = 6", "9 mod 6 = 3", "6 mod 3 = 0", "Finished with 3"]) $ runWriter $ gcd' 9 24,
                        assertEqualTestCase __LINE__ 1 $ fst $ runWriter $ gcd' 8 3,
                        assertEqualTestCase __LINE__ [1, 2, 3, 4, 1, 2, 3] $ fromDiffList $ toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3],
                        assertEqualTestCase __LINE__ (1, ["Finished with 1","2 mod 1 = 0","3 mod 2 = 1","8 mod 3 = 2"]) $ id *** fromDiffList $ runWriter $ gcdReverse 8 3,
                        assertEqualTestCase __LINE__ 19 $ addStuff 3,
                        assertEqualTestCase __LINE__ (5,[8,2,1]) $ runState stackManip [5,8,2,1],
                        assertEqualTestCase __LINE__ ((),[8,3,0,2,1,0]) $ runState stackStuff [9,0,2,1,0],
                        assertEqualTestCase __LINE__ ((),[8,3,2,1,0]) $ runState moreStack [100,0,2,1,0],
                        assertEqualTestCase __LINE__ [8, 3, 1] $ fst . runState stackyStack $ [1, 2, 3],
                        assertEqualTestCase __LINE__ [9, 2, 1] $ fst . runState stackyStack $ [8, 3, 1],
                        assertEqualTestCase __LINE__ (True,False,True) $ evalState threeCoins' $ mkStdGen 33,
                        assertEqualTestCase __LINE__ "boom!" $ strMsg ("boom!" :: String),
                        assertEqualTestCase __LINE__ (Left "boom" :: Num a => Either String a) $ Left "boom" >>= return . (+1),
                        assertEqualTestCase __LINE__ (Left "no way!" :: Num a => Either String a) $ Right 100 >>= const (Left "no way!"),
                        assertEqualTestCase __LINE__ (Right 103 :: Num a => Either a a) $ Right 3 >>= return . (+100),
                        assertEqualTestCase __LINE__ (Right (2,0)) $ emptyPole >>= landLeft 2,
                        assertEqualTestCase __LINE__ (Left "Fell with 10 birds perched on the left and 3 birds perched on the right.") $ return (0,3) >>= landLeft 10,
                        assertEqualTestCase __LINE__ (Right (2,1)) $ return (0,0) >>= landRight 1 >>= landLeft 2,
                        assertEqualTestCase __LINE__ (Left "Fell from the start") $ Left "Fell from the start" >>= landLeft 2,
                        assertEqualTestCase __LINE__ (Right (2,4)) $ emptyPole >>= landRight 2 >>= landLeft 2 >>= landRight 2,
                        assertEqualTestCase __LINE__ (Left "Fell with 0 birds perched on the left and 4 birds perched on the right.") $ emptyPole >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2),
                        assertEqualTestCase __LINE__ (Left "Fell after slipping on a banana.") $ emptyPole >>= landLeft 1 >>= banana >>= landRight 1,
                        assertEqualTestCase __LINE__ (Left "Fell for no reason") $ emptyPole >>= landLeft 1 >> Left "Fell for no reason" >>= landRight 1,
                        {- end -}
                        assertEqualTestCase __LINE__ True True]
  where
    assertEqualTestCase :: (Show a, Eq a, Num b) => b -> a -> a -> Test
    assertEqualTestCase w x y = TestCase $ assertEqual (printf "Line: %s" (show w)) x y

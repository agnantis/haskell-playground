import Test.QuickCheck
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))
import Data.List (permutations)
import FreeNumber


largeNumber = 3000000

testList, testList1 :: [Int]
testList1 = [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] 
testList = concatMap (replicate 10000) list -- concat . permutations $ list
 where
   list = [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6]


testListLarge :: Int -> [Int]
testListLarge n = [0..n] ++ [n+2 .. 3*n]

testminfreeNaive1 :: Assertion
testminfreeNaive1 = minfreeNaive [] @?= 0

testminfreeNaive2 :: Assertion
testminfreeNaive2 = minfreeNaive testList @?= 15

testminfreeNaive3 :: Assertion
testminfreeNaive3 = minfreeNaive (testListLarge largeNumber) @?= largeNumber+1

prop1_minfreeNaive :: [Int] -> Bool
prop1_minfreeNaive xs = not $ elem res xs
  where
    res = minfreeNaive $ filter (>=0) xs

prop2_minfreeNaive :: Int -> Bool
prop2_minfreeNaive n | n < 0     = True
                | otherwise = minfreeNaive [0..n] == (n+1)

testminfreeArray :: Assertion
testminfreeArray = minfreeArray [] @?= 0

testminfreeArray2 :: Assertion
testminfreeArray2 = minfreeArray testList @?= 15

testminfreeArray3 :: Assertion
testminfreeArray3 = minfreeArray (testListLarge largeNumber) @?= largeNumber+1

prop1_minfreeArray :: [Int] -> Bool
prop1_minfreeArray xs = not $ elem res xs
  where
    res = minfreeArray $ filter (>=0) xs

prop2_minfreeArray :: Int -> Bool
prop2_minfreeArray n | n < 0     = True
                 | otherwise = minfreeArray [0..n] == (n+1)

testminfreeDnCArray :: Assertion
testminfreeDnCArray = minfreeDnC [] @?= 0

testminfreeDnCArray2 :: Assertion
testminfreeDnCArray2 = minfreeDnC testList1 @?= 15

testminfreeDnCArray3 :: Assertion
testminfreeDnCArray3 = minfreeDnC (testListLarge largeNumber) @?= largeNumber+1

prop1_minfreeDnC :: [Int] -> Bool
prop1_minfreeDnC xs = not $ elem res xs
  where
    res = minfreeDnC $ filter (>=0) xs

prop2_minfreeDnC :: Int -> Bool
prop2_minfreeDnC n | n < 0     = True
                  | otherwise = minfreeDnC [0..n] == (n+1)

testminfreeSTArray :: Assertion
testminfreeSTArray = minfreeSTArray [] @?= 0

testminfreeSTArray2 :: Assertion
testminfreeSTArray2 = minfreeSTArray testList @?= 15

testminfreeSTArray3 :: Assertion
testminfreeSTArray3 = minfreeSTArray (testListLarge largeNumber) @?= largeNumber+1

prop1_minfreeSTArray :: [Int] -> Bool
prop1_minfreeSTArray xs = not $ elem res xs
  where
    res = minfreeSTArray $ filter (>=0) xs

prop2_minfreeSTArray :: Int -> Bool
prop2_minfreeSTArray n | n < 0     = True
                | otherwise = minfreeSTArray [0..n] == (n+1)

testNaive :: TestTree
testNaive = testGroup "minfreeNaive tests - naive approach"
  [ testCase "empty list should return 0" testminfreeNaive1
  , testCase "not empty list should return a value" testminfreeNaive2
  , testCase "[0..n] ++ [n+2..] == n+1" testminfreeNaive3
  , testProperty "result should not exist in input list" prop1_minfreeNaive
  , testProperty "result for [0..n] == n+1" prop2_minfreeNaive 
  ]

testArray :: TestTree
testArray = testGroup "minfree tests - array approach"
  [ testCase "empty list should return 0" testminfreeArray
  , testCase "not empty list should return a value" testminfreeArray2
  , testCase "[0..n] ++ [n+2..] == n+1" testminfreeArray3
  , testProperty "result should not exist in input list" prop1_minfreeArray
  , testProperty "result for [0..n] == n+1" prop2_minfreeArray 
  ]

testDnC :: TestTree
testDnC = testGroup "minfree tests - divide approach"
  [ testCase "empty list should return 0" testminfreeDnCArray
  , testCase "not empty list should return a value" testminfreeDnCArray2
  , testCase "[0..n] ++ [n+2..] == n+1" testminfreeDnCArray3
  --, testProperty "result should not exist in input list" prop1_minfreeDnC 
  , testProperty "result for [0..n] == n+1" prop2_minfreeDnC 
  ]

testSTArray :: TestTree
testSTArray = testGroup "minfree tests - ST Arrays"
  [ testCase "empty list should return 0" testminfreeSTArray
  , testCase "not empty list should return a value" testminfreeSTArray2
  , testCase "[0..n] ++ [n+2..] == n+1" testminfreeSTArray3
  , testProperty "result should not exist in input list" prop1_minfreeSTArray
  , testProperty "result for [0..n] == n+1" prop2_minfreeSTArray
  ]

main :: IO ()
main = defaultMain $ testGroup "test everything" 
  [ testNaive
  , testArray
  , testDnC
  , testSTArray
  ]

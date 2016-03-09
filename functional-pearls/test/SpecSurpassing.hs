import Test.QuickCheck
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))
import Data.List (permutations)
import Surpassing


testNaive1 :: Assertion
testNaive1 = mscNaive "GENERATION" @?= 6

prop1Naive :: [Int] -> Bool
prop1Naive [] = (mscNaive ([]:: [Int])) == 0
prop1Naive xs = mscNaive xs < length xs

testNaive :: TestTree
testNaive = testGroup "Surpassing - naive approach"
  [ testCase "Generation shoud return 6" testNaive1
  , testProperty "result should be less then non-empty inpit" prop1Naive
  ]


main :: IO ()
main = defaultMain $ testGroup "test everything" 
  [ testNaive
  ]

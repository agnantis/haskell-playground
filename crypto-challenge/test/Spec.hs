import Test.QuickCheck
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))
import Lib 


testHex2Base64 :: Assertion
testHex2Base64 = hex2base64 input @?= output
  where
    input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

testFixedOR :: Assertion
testFixedOR = fixedXOR input1 input2 @?= Just output
  where
    input1 = "1c0111001f010100061a024b53535009181c"
    input2 = "686974207468652062756c6c277320657965"
    output = "746865206b696420646f6e277420706c6179"


testChallenges :: TestTree
testChallenges = testGroup "All Challenges"
  [ testCase "Challenge 1 - Test hex to Base64" testHex2Base64
  , testCase "Challenge 2 - Test Fixed XOR" testFixedOR
  ]


main :: IO ()
main = defaultMain testChallenges

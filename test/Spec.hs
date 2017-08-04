module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Golf

skipsSuite :: TestTree
skipsSuite = testGroup "0three rec poly TEST"
              [ testGroup "Test of --skips-- function" $
                  [testCase "test 1" $ skips "ABCD" @?=["ABCD","BD","C","D"]
                  ,testCase "test 2" $ skips "hello!" @?=["hello!","el!","l!","l","o","!"]
                  ,testCase "test 3" $ skips [1] @?=[[1]]
                  ,testCase "test 4" $ skips [True,False] @?= [[True,False], [False]]
                  ]

            , testGroup "Test of --localMaxima-- function"
                  [testCase "test 1" $ localMaxima [2,9,5,6,1] @?=[9,6]
                  ,testCase "test 2" $ localMaxima [2,3,4,1,5] @?=[4]
                  ,testCase "test 3" $ localMaxima [1,2,3,4,5] @?=[]
                  ]
              ]

main = defaultMain skipsSuite


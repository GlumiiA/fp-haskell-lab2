module Main (main) where

import Test.HUnit
import Problem8
import Problem22
import Data.List (sort)

-- Тесты для задачи 8

testLargestProduct :: Test
testLargestProduct = TestList
  [ "Рекурсия даёт правильный результат" ~:
      largestProduct1 13 digits ~?= 23514624000

  , "Все реализации дают одинаковый результат" ~:
      let r1 = largestProduct1 13 digits
          r2 = largestProduct2 13 digits
          r3 = largestProduct3 13 digits
          r4 = largestProduct4 13 digits
      in (r1, r2, r3, r4) ~?= (r1, r1, r1, r4)
  ]

-- Тесты для задачи 22

testNamesScores :: Test
testNamesScores = TestList
  [ "Значение имени COLIN должно быть 53" ~:
      nameValue "COLIN" ~?= 53

  , "Все реализации totalScore возвращают одинаковое значение" ~:
      let names = ["COLIN", "MARY", "PATRICIA"]
          sorted = sort names
          r1 = totalScore1 sorted
          r2 = totalScore2 sorted
          r3 = totalScore3 sorted
          r4 = totalScore4 sorted
          r5 = totalScore5 sorted
      in (r1, r2, r3, r4, r5) ~?= (r1, r1, r1, r1, r1)
  ]


main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [ TestLabel "Problem 8" testLargestProduct
    , TestLabel "Problem 22" testNamesScores
    ]
  return ()

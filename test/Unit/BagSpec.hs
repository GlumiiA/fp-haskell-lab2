{-# LANGUAGE ScopedTypeVariables #-}

module Unit.BagSpec (bagTests) where

import Bag.API
import Bag.Internal
import Test.HUnit
import Types ()

-- Тест добавления и подсчёта
testInsert :: Test
testInsert = TestCase $ do
  let b = insertB (1 :: Int) (emptyB :: Bag Int)
  countB 1 b @?= 1
  let b2 = insertB (1 :: Int) b
  countB 1 b2 @?= 2

-- Тест удаления
testDelete :: Test
testDelete = TestCase $ do
  let b = insertB (2 :: Int) $ insertB (2 :: Int) (emptyB :: Bag Int)
  let b1 = deleteB (2 :: Int) b
  countB 2 b1 @?= 1
  let b2 = deleteB (2 :: Int) b1
  countB 2 b2 @?= 0

-- Тест map
testMap :: Test
testMap = TestCase $ do
  let b = insertB (1 :: Int) (emptyB :: Bag Int)
  let b2 = mapB ((+ 1) :: Int -> Int) b
  countB 2 b2 @?= 1

-- Тест filter
testFilter :: Test
testFilter = TestCase $ do
  let b = Prelude.foldl (flip insertB) (emptyB :: Bag Int) [1, 2, 3, 4]
      b2 = filterB (even :: Int -> Bool) b
  countB 2 b2 @?= 1
  countB 4 b2 @?= 1
  countB 1 b2 @?= 0

-- Тест Monoid
testMonoid :: Test
testMonoid = TestCase $ do
  let b1 = insertB (1 :: Int) (emptyB :: Bag Int)
      b2 = insertB (1 :: Int) (emptyB :: Bag Int)
      b3 = mappendM b1 b2
  countB 1 b3 @?= 2

-- Тест Eq
testEq :: Test
testEq = TestCase $ do
  let b1 = insertB (1 :: Int) $ insertB (2 :: Int) (emptyB :: Bag Int)
      b2 = insertB (2 :: Int) $ insertB (1 :: Int) (emptyB :: Bag Int)
  assertBool "bags should be equal" (b1 == b2)

bagTests :: Test
bagTests =
  TestList
    [ testInsert,
      testDelete,
      testMap,
      testFilter,
      testMonoid,
      testEq
    ]

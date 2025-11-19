{-# LANGUAGE ScopedTypeVariables #-}

module Unit.HashMapSpec (hashTests) where

import Test.HUnit
import HashMap.API
import HashMap.Internal
import Types()

-- Тест вставки и поиска
testInsertLookup :: Test
testInsertLookup = TestCase $ do
    let hm = insertH (1 :: Int) (100 :: Int) (emptyH :: HashMap Int Int)
    lookupH 1 hm @?= Just 100
    let hm2 = insertH (1 :: Int) (200 :: Int) hm
    lookupH 1 hm2 @?= Just 200

-- Тест удаления
testDelete :: Test
testDelete = TestCase $ do
    let hm = insertH (2 :: Int) (50 :: Int) (emptyH :: HashMap Int Int)
        hm1 = deleteH (2 :: Int) hm
    lookupH 2 hm1 @?= Nothing
    let hm2 = deleteH (3 :: Int) hm1  -- удалить несуществующий ключ
    lookupH 2 hm2 @?= Nothing

-- Тест foldl и foldr
testFold :: Test
testFold = TestCase $ do
    let hm = Prelude.foldl (\acc (k,v) -> insertH (k :: Int) (v :: Int) acc) (emptyH :: HashMap Int Int) ([(1,10),(2,20)] :: [(Int,Int)])
        sumVals = foldlH (\s (_,v) -> s + v) (0 :: Int) hm
        sumValsR = foldrH (\(_,v) s -> s + v) (0 :: Int) hm
    sumVals @?= 30
    sumValsR @?= 30

-- Тест map
testMap :: Test
testMap = TestCase $ do
    let hm = insertH (1 :: Int) (5 :: Int) $ insertH (2 :: Int) (10 :: Int) (emptyH :: HashMap Int Int)
        hm2 = mapH ((*2) :: Int -> Int) hm
    lookupH 1 hm2 @?= Just 10
    lookupH 2 hm2 @?= Just 20

-- Тест filter
testFilter :: Test
testFilter = TestCase $ do
    let hm = Prelude.foldl (\acc (k,v) -> insertH (k :: Int) (v :: Int) acc) (emptyH :: HashMap Int Int) ([(1,10),(2,20),(3,30)] :: [(Int,Int)])
        hm2 = filterH (\(k,_) -> k `mod` (2 :: Int) == 1) hm
    lookupH 1 hm2 @?= Just 10
    lookupH 3 hm2 @?= Just 30
    lookupH 2 hm2 @?= Nothing

-- Тест коллизий (несколько ключей с одинаковым hash)
testCollisions :: Test
testCollisions = TestCase $ do
    let hm = insertH (1 :: Int) (100 :: Int) $ insertH (1 + initialCapacity) (200 :: Int) (emptyH :: HashMap Int Int)
    lookupH 1 hm @?= Just 100
    lookupH (1 + initialCapacity) hm @?= Just 200

hashTests :: Test
hashTests = TestList
    [ testInsertLookup
    , testDelete
    , testFold
    , testMap
    , testFilter
    , testCollisions
    ]
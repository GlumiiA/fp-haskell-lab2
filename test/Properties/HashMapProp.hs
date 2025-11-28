
module Properties.HashMapProp (propTests) where

import Test.HUnit
import Test.QuickCheck
import HashMap.API
import HashMap.Internal
import Types()
import Data.List (nubBy)

-- QuickCheck arguments (limit tests/sizes to speed CI)
qcArgs :: Args
qcArgs = stdArgs { maxSuccess = 20, maxSize = 10 }

fromListHM :: [(Int,Int)] -> HashMap Int Int
fromListHM = Prelude.foldl (\acc (k,v) -> insertH k v acc) (emptyH :: HashMap Int Int)

prop_insert_lookup :: Int -> Int -> [(Int,Int)] -> Property
prop_insert_lookup k v kvs =
    property $ lookupH k (insertH k v (fromListHM kvs)) == Just v

prop_map_values :: [(Int,Int)] -> Property
prop_map_values kvs =
    let uniqueKvs = nubBy (\(k1,_) (k2,_) -> k1 == k2) kvs  -- remove duplicate keys
        hm = fromListHM uniqueKvs
        mappedHm = mapH ((+1) :: Int -> Int) hm
    in property $ all (\(k,v) -> lookupH k mappedHm == Just (v + 1)) uniqueKvs

propTests :: Test
propTests = TestCase $ do
    r1 <- quickCheckWithResult qcArgs prop_insert_lookup
    case r1 of
        Success{} -> return ()
        _ -> assertFailure "prop_insert_lookup failed"
    r2 <- quickCheckWithResult qcArgs prop_map_values
    case r2 of
        Success{} -> return ()
        _ -> assertFailure "prop_map_values failed"


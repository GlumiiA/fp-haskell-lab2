
module Properties.BagProp (propTests) where

import Test.HUnit
import Test.QuickCheck
import Bag.API
import Bag.Internal
import Types()

-- Build a Bag from a list of Ints
fromListBag :: [Int] -> Bag Int
fromListBag = Prelude.foldl (\acc x -> insertB x acc) (emptyB :: Bag Int)

-- Total number of elements in a Bag (sum of counts)
totalCount :: Bag Int -> Int
totalCount b = foldlB (\s (_,n) -> s + n) 0 b

prop_insert_increases_count :: Int -> [Int] -> Property
prop_insert_increases_count x xs =
	let b = fromListBag xs
	in counterexample ("x=" ++ show x ++ ", xs=" ++ show xs) $
			 countB x (insertB x b) === countB x b + 1

prop_totalCount_preserved_by_map :: [Int] -> Property
prop_totalCount_preserved_by_map xs =
	let b = fromListBag xs
	in totalCount (mapB (+1) b) === totalCount b

prop_monoid_identity :: [Int] -> Property
prop_monoid_identity xs =
	let b = fromListBag xs
	in property $ (mappendM b memptyM == b) && (mappendM memptyM b == b)

prop_monoid_associative :: [Int] -> [Int] -> [Int] -> Property
prop_monoid_associative xs ys zs =
	let a = fromListBag xs; b = fromListBag ys; c = fromListBag zs
	in property $ (mappendM (mappendM a b) c) == (mappendM a (mappendM b c))

prop_monoid_identity_std :: [Int] -> Property
prop_monoid_identity_std xs =
	let b = fromListBag xs
	in property $ (b <> mempty) == b && (mempty <> b) == b

prop_monoid_associative_std :: [Int] -> [Int] -> [Int] -> Property
prop_monoid_associative_std xs ys zs =
	let a = fromListBag xs; b = fromListBag ys; c = fromListBag zs
	in property $ ((a <> b) <> c) == (a <> (b <> c))

-- Run QuickCheck properties and return as an HUnit Test
propTests :: Test
propTests = TestCase $ do
	r1 <- quickCheckResult prop_insert_increases_count
	case r1 of
		Success{} -> return ()
		_ -> assertFailure "prop_insert_increases_count failed"
	r2 <- quickCheckResult prop_totalCount_preserved_by_map
	case r2 of
		Success{} -> return ()
		_ -> assertFailure "prop_totalCount_preserved_by_map failed"
	r3 <- quickCheckResult prop_monoid_identity
	case r3 of
		Success{} -> return ()
		_ -> assertFailure "prop_monoid_identity failed"
	r4 <- quickCheckResult prop_monoid_associative
	case r4 of
		Success{} -> return ()
		_ -> assertFailure "prop_monoid_associative failed"
	r5 <- quickCheckResult prop_monoid_identity_std
	case r5 of
		Success{} -> return ()
		_ -> assertFailure "prop_monoid_identity_std failed"
	r6 <- quickCheckResult prop_monoid_associative_std
	case r6 of
		Success{} -> return ()
		_ -> assertFailure "prop_monoid_associative_std failed"


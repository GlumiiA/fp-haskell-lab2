module Bag.API where

import Bag.Internal
import Types

class BagClass b where
  emptyB :: b a
  insertB :: (Eq a, Hashable a) => a -> b a -> b a
  deleteB :: (Eq a, Hashable a) => a -> b a -> b a
  countB :: (Eq a, Hashable a) => a -> b a -> Int
  mapB :: (Eq a, Hashable a) => (a -> a) -> b a -> b a
  filterB :: (Eq a, Hashable a) => (a -> Bool) -> b a -> b a
  foldlB :: (x -> (a, Int) -> x) -> x -> b a -> x
  foldrB :: ((a, Int) -> x -> x) -> x -> b a -> x

instance BagClass Bag where
  emptyB = emptyBag
  insertB = insertBag
  deleteB = deleteBag
  countB = countBag
  mapB = mapBag
  filterB = filterBag
  foldlB = foldlBag
  foldrB = foldrBag

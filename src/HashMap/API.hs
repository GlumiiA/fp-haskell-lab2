module HashMap.API where

import HashMap.Internal
import Types

-- Класс Hashmap с методами
class Hashmap m where
  emptyH   :: m k v
  insertH  :: (Eq k, Hashable k) => k -> v -> m k v -> m k v
  deleteH  :: (Eq k, Hashable k) => k -> m k v -> m k v
  lookupH  :: (Eq k, Hashable k) => k -> m k v -> Maybe v
  mapH     :: (v -> w) -> m k v -> m k w
  filterH  :: ((k,v) -> Bool) -> m k v -> m k v
  foldlH   :: (a -> (k,v) -> a) -> a -> m k v -> a
  foldrH   :: ((k,v) -> a -> a) -> a -> m k v -> a

-- Instance для HashMap
instance Hashmap HashMap where
  emptyH  = empty
  insertH = insert
  deleteH = delete
  lookupH = HashMap.Internal.lookup
  mapH    = mapHash
  filterH = filterHash
  foldlH  = foldlHash
  foldrH  = foldrHash

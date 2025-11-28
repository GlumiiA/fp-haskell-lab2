module HashMap.Internal where

import qualified Data.Vector as V
import Types

data Bucket k v
  = Empty
  | Deleted
  | Occupied k v
  deriving (Show, Eq)

data HashMap k v = HM
  { buckets :: V.Vector (Bucket k v),
    size :: Int
  }
  deriving (Show)

initialCapacity :: Int
initialCapacity = 16

empty :: HashMap k v
empty = HM {buckets = V.replicate initialCapacity Empty, size = 0}

loadFactor :: HashMap k v -> Float
loadFactor hm = fromIntegral (size hm) / fromIntegral (V.length (buckets hm))

tableProbe :: Int -> Int -> Int
tableProbe h i = h + i

findSlot :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe Int
findSlot k hm = go 0
  where
    cap = V.length (buckets hm)
    h0 = hash k `mod` cap
    go i
      | i >= cap = Nothing
      | otherwise =
          let idx = tableProbe h0 i `mod` cap
           in case buckets hm V.! idx of
                Empty -> Nothing
                Deleted -> go (i + 1)
                Occupied k' _ -> if k == k' then Just idx else go (i + 1)

findInsertPos :: (Eq k, Hashable k) => k -> HashMap k v -> Int
findInsertPos k hm = go 0
  where
    cap = V.length (buckets hm)
    h0 = hash k `mod` cap
    go i =
      let idx = tableProbe h0 i `mod` cap
       in case buckets hm V.! idx of
            Empty -> idx
            Deleted -> idx
            Occupied k' _ -> if k == k' then idx else go (i + 1)

-- Lookup
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k hm = case findSlot k hm of
  Just idx -> case buckets hm V.! idx of
    Occupied _ v -> Just v
    _ -> Nothing
  Nothing -> Nothing

-- Insert
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k v hm =
  let idx = findInsertPos k hm
      vec = buckets hm V.// [(idx, Occupied k v)]
      newSize = case buckets hm V.! idx of
        Empty -> size hm + 1
        Deleted -> size hm + 1
        Occupied _ _ -> size hm
   in HM {buckets = vec, size = newSize}

-- Map over values
mapHash :: (v -> w) -> HashMap k v -> HashMap k w
mapHash f hm =
  let vec' = V.map mapBucket (buckets hm)
      mapBucket Empty = Empty
      mapBucket Deleted = Deleted
      mapBucket (Occupied k v) = Occupied k (f v)
   in HM {buckets = vec', size = size hm}

-- Delete
delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k hm = case findSlot k hm of
  Just idx ->
    let vec = buckets hm V.// [(idx, Deleted)]
     in HM {buckets = vec, size = size hm - 1}
  Nothing -> hm

-- Filter
filterHash :: ((k, v) -> Bool) -> HashMap k v -> HashMap k v
filterHash p hm =
  let vec' = V.map filterBucket (buckets hm)
      filterBucket b@(Occupied k v) = if p (k, v) then b else Deleted
      filterBucket x = x
      newSize = V.length $ V.filter isOccupied vec'
      isOccupied (Occupied _ _) = True
      isOccupied _ = False
   in HM {buckets = vec', size = newSize}

-- Fold left
foldlHash :: (a -> (k, v) -> a) -> a -> HashMap k v -> a
foldlHash f acc hm = V.foldl folder acc (buckets hm)
  where
    folder a (Occupied k v) = f a (k, v)
    folder a _ = a

-- Fold right
foldrHash :: ((k, v) -> a -> a) -> a -> HashMap k v -> a
foldrHash f acc hm = V.foldr folder acc (buckets hm)
  where
    folder (Occupied k v) a = f (k, v) a
    folder _ a = a

module Bag.Internal where

import Types
import HashMap.API
import HashMap.Internal (HashMap, size)
import Data.Maybe (fromMaybe)


-- Bag data type, хранит HashMap a Int
newtype Bag a = Bag { getBag :: HashMap a Int }

-- Пустой Bag
emptyBag :: Bag a
emptyBag = Bag emptyH

-- Вставка элемента
insertBag :: (Eq a, Hashable a) => a -> Bag a -> Bag a
insertBag x (Bag hm) =
  let newCount = maybe 1 (+1) (lookupH x hm)
  in Bag (insertH x newCount hm)

-- Удаление элемента
deleteBag :: (Eq a, Hashable a) => a -> Bag a -> Bag a
deleteBag x (Bag hm) =
  case lookupH x hm of
    Nothing -> Bag hm
    Just n  -> if n > 1
               then Bag (insertH x (n-1) hm)
               else Bag (deleteH x hm)

-- Подсчёт количества
countBag :: (Eq a, Hashable a) => a -> Bag a -> Int
countBag x (Bag hm) = fromMaybe 0 (lookupH x hm)  -- HLINT ignore "Use fromMaybe"

-- Применение функции к каждому элементу
mapBag :: (Eq a, Hashable a) => (a -> a) -> Bag a -> Bag a
mapBag f (Bag hm) = foldlH step emptyBag hm
  where
    step (Bag acc) (k,n) = Bag (insertH (f k) n acc)

-- Фильтрация элементов
filterBag :: (Eq a, Hashable a) => (a -> Bool) -> Bag a -> Bag a
filterBag p (Bag hm) = Bag (filterH (\(k,_) -> p k) hm)

-- Свёртка влево
foldlBag :: (b -> (a, Int) -> b) -> b -> Bag a -> b
foldlBag f acc (Bag hm) = foldlH f acc hm

-- Свёртка вправо
foldrBag :: ((a, Int) -> b -> b) -> b -> Bag a -> b
foldrBag f acc (Bag hm) = foldrH f acc hm

-- Класс моноида для Bag
class MyMonoid m where
  memptyM  :: m
  mappendM :: m -> m -> m

instance (Eq a, Hashable a) => MyMonoid (Bag a) where
  memptyM = emptyBag
  mappendM (Bag hm1) (Bag hm2) = foldlH step (Bag hm1) hm2
    where
      step (Bag acc) (k,v) =
        case lookupH k acc of
          Nothing -> Bag (insertH k v acc)
          Just n  -> Bag (insertH k (n+v) acc)

-- Экземпляр Eq для Bag
instance (Eq a, Hashable a) => Eq (Bag a) where
  (Bag hm1) == (Bag hm2)
    | size hm1 /= size hm2 = False
    | otherwise             = foldlH check True hm1
    where
      check acc (k,v)
        | not acc  = False
        | otherwise = case lookupH k hm2 of
                        Just v2 -> v == v2
                        Nothing -> False

instance (Eq a, Hashable a) => Semigroup (Bag a) where
  (<>) = mappendM

instance (Eq a, Hashable a) => Monoid (Bag a) where
  mempty = memptyM
  mappend = (<>)

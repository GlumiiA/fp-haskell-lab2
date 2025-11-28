{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

class Hashable k where
  hash :: k -> Int

-- Concrete Hashable instance for Int to avoid orphan instances across tests
instance Hashable Int where
  hash = id

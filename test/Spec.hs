module Main where

import Test.HUnit
import Unit.BagSpec (bagTests)
import Unit.HashMapSpec (hashTests)
import qualified Properties.BagProp as PB
import qualified Properties.HashMapProp as PH

main :: IO ()
main = do
  _ <- runTestTT $ TestList [bagTests, hashTests, PB.propTests, PH.propTests]
  return ()

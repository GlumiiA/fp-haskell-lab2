module Main (main) where

import Problem8
import Problem22

main :: IO ()
main = do
  putStrLn "Задача #8: Largest Product in a Series"
  let n = 13
  putStrLn $ "Рекурсия:          " ++ show (largestProduct1 n digits)
  putStrLn $ "Списковое выражение: " ++ show (largestProduct2 n digits)
  putStrLn $ "Через fold:         " ++ show (largestProduct3 n digits)
  putStrLn "Задача #22: Names Scores"
  names <- loadNames "names.txt"
  putStrLn $ "1. Рекурсия:            " ++ show (totalScore1 names)
  putStrLn $ "2. Хвостовая рекурсия:  " ++ show (totalScore2 names)
  putStrLn $ "3. Через fold:          " ++ show (totalScore3 names)
  putStrLn $ "4. Через map:           " ++ show (totalScore4 names)
  putStrLn $ "5. Ленивый список:      " ++ show (totalScore5 names)

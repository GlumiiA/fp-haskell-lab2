# Ибрагимова Айгуль — P3311

ИСУ: 408675

## Ссылки на задачи

* [Problem 8 — Largest Product in a Series](https://projecteuler.net/problem=8)
* [Problem 22 — Names Scores](https://projecteuler.net/problem=22)

---

# Выполнение

## Problem 8

### Решение на Python

**Описание:**
Реализовано скользящее окно длиной 13 символов. Если в окне встречается `0`, окно пропускается.
Вычисляется произведение всех цифр и запоминается наибольшее.

```python
from math import prod

num_str = (
    "73167176531330624919225119674426574742355349194934"
    "96983520312774506326239578318016984801869478851843"
    "85861560789112949495459501737958331952853208805511"
    "12540698747158523863050715693290963295227443043557"
    "66896648950445244523161731856403098711121722383113"
    "62229893423380308135336276614282806444486645238749"
    "30358907296290491560440772390713810515859307960866"
    "70172427121883998797908792274921901699720888093776"
    "65727333001053367881220235421809751254540594752243"
    "52584907711670556013604839586446706324415722155397"
    "53697817977846174064955149290862569321978468622482"
    "83972241375657056057490261407972968652414535100474"
    "82166370484403199890008895243450658541227588666881"
    "16427171479924442928230863465674813919123162824586"
    "17866458359124566529476545682848912883142607690042"
    "24219022671055626321111109370544217506941658960408"
    "07198403850962455444362981230987879927244284909188"
    "84580156166097919133875499200524063689912560717606"
    "05886116467109405077541002256983155200055935729725"
    "71636269561882670428252483600823257530420752963450"
)

k = 13

def result(k, num_str):
    max_product = 0
    best_digits = ""
    for i in range(len(num_str) - k + 1):
        window = num_str[i:i + k]
        if '0' in window:
            continue
        product = prod(int(d) for d in window)
        if product > max_product:
            max_product = product
            best_digits = window
    return best_digits, max_product

best_digits, max_product = result(k, num_str)

print("Наибольшее произведение из 13 подряд идущих цифр:")
print(best_digits, "=", max_product)
```

**Результат выполнения:**

```
5576689664895 = 23514624000
```

---

### Решение на Haskell

#### Рекурсивный подход

Сравниваем произведение первых `n` цифр с результатом для хвоста строки.

```haskell
largestProduct1 :: Int -> String -> Int
largestProduct1 n xs
  | length xs < n = 0
  | otherwise =
      let firstProd = product (toInts (take n xs))
          restProd  = largestProduct1 n (drop 1 xs)
      in max firstProd restProd
```

---

#### Через списковое выражение

Создаём список всех окон длины `n` и берём максимум произведений.

```haskell
largestProduct2 :: Int -> String -> Int
largestProduct2 n xs =
  maximum [product (toInts (take n (drop i xs))) | i <- [0 .. length xs - n]]
```

---

#### Через `tails`

Перебираем все хвосты строки, берём первые `n` цифр и считаем произведение.

```haskell
largestProduct3 :: Int -> String -> Int
largestProduct3 n xs =
  maximum [product (map digitToInt (take n t)) | t <- tails xs, length t >= n]
```

---

#### Вспомогательная функция

Преобразует строку цифр в список чисел.

```haskell
toInts :: String -> [Int]
toInts = map digitToInt
```

---

#### Модульная реализация через fold, filter и map

Генерируем все окна длиной `n`, фильтруем окна с нулём, вычисляем произведения и берём максимум через `foldl`.

```haskell
largestProduct4 :: Int -> String -> Int
largestProduct4 n xs =
    let
        windows = [take n (drop i xs) | i <- [0..length xs - n]]
        filtered = filter (notElem '0') windows
        products = map (product . toInts) filtered
    in
        foldl max 0 products
```
---

## Problem 22

### Решение на Python

**Описание:**
Читаем файл с именами, сортируем, вычисляем значение каждого имени и общий результат.

```python
with open("0022_names.txt", "r") as f:
    content = f.read()

names = content.replace('"', '').split(',')
names.sort()

def name_value(name):
    return sum(ord(char) - ord('A') + 1 for char in name)

total_score = sum((i + 1) * name_value(name) for i, name in enumerate(names))

print(total_score)
```

---

### Решение на Haskell

#### Чтение и сортировка имён

```haskell
loadNames :: String -> IO [String]
loadNames path = do
  content <- readFile path
  let names = sort (read ("[" ++ content ++ "]") :: [String])
  return names
```

---

#### Вычисление "веса" имени

```haskell
nameValue :: String -> Int
nameValue = sum . map (\c -> ord c - ord 'A' + 1)
```

---

#### 1. Обычная рекурсия

```haskell
totalScore1 :: [String] -> Int
totalScore1 names = helper names 1
  where
    helper [] _ = 0
    helper (x:xs) i = nameValue x * i + helper xs (i + 1)
```

---

#### 2. Хвостовая рекурсия

```haskell
totalScore2 :: [String] -> Int
totalScore2 names = go names 1 0
  where
    go [] _ acc = acc
    go (x:xs) i acc = go xs (i + 1) (acc + nameValue x * i)
```

---

#### 3. Модульный вариант с `fold`

```haskell
totalScore3 :: [String] -> Int
totalScore3 names =
  let nameScores = zipWith (\i n -> (i + 1) * nameValue n) [0..] names
  in foldl (+) 0 nameScores
```

---

#### 4. Использование `map`

```haskell
totalScore4 :: [String] -> Int
totalScore4 names =
  let nameValues = map nameValue names
      scores = zipWith (*) nameValues [1..]
  in sum scores
```

---

#### 5. Ленивые бесконечные списки

```haskell
totalScore5 :: [String] -> Int
totalScore5 names = sum $ zipWith (*) (map nameValue names) [1..]
```

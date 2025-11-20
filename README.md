# Работа: Лабораторная работа №2 — Реализация структур данных (Bag, HashMap) на Haskell
**Ибрагимова Айгуль P3311 408675**

**Требования к разработанному ПО**

- Реализовать неизменяемые структуры данных: HashMap и Bag (multiset).
- API должно предоставлять операции: добавление, удаление, фильтрация, отображение (map), свёртки (foldl/foldr), подсчёт количества элементов (для Bag).
- Структура Bag должна обладать семантикой моноида (бинарная операция объединения + нейтральный элемент).
- Структуры должны быть полиморфными и реализованы в функциональном стиле.
- Библиотека должна быть проверена unit-тестами (HUnit) и property-based тестами (QuickCheck).
- Внешние детали реализации (модули Internal) не должны «протекать» в публичный API.

**Ключевые элементы реализации (минимальные комментарии)**

- Модель хранения HashMap:
  - Модуль: `src/HashMap/Internal.hs`
  - Реализация: открытая адресация в векторе (`Data.Vector`) с состояниями корзин: `Empty | Deleted | Occupied k v`.
  - Основные операции: `lookup`, `insert`, `delete`, `mapHash`, `filterHash`, `foldlHash`, `foldrHash`.
  - Поле `size` хранит количество занятых ячеек для быстрого сравнения размеров.

- Bag (multiset):
  - Модуль: `src/Bag/Internal.hs` (интерфейс в `src/Bag/API.hs`).
  - Bag — обёртка над `HashMap a Int`, где значение — счётчик вхождений.
  - Операции: `emptyBag`, `insertBag`, `deleteBag`, `countBag`, `mapBag`, `filterBag`, `foldlBag`, `foldrBag`.
  - Моноид: реализованы `memptyM`/`mappendM` (локальная сигнатура `MyMonoid`) и стандартные экземпляры `Semigroup`/`Monoid` (`<>`/`mempty`) для `Bag`.
  - Экземпляр `Eq (Bag a)` реализован через сравнение размеров и затем проверку соответствия счётчиков через `lookup` — эффективный (не приводит к спискам и сортировке).

- API-модули:
  - `src/HashMap/API.hs` и `src/Bag/API.hs` содержат классы и функции, которые экспортируются пользователю.
 
  **Примеры кода (кратко)**

  ```haskell
  -- Представление HashMap (внутренне)
  data Bucket k v = Empty | Deleted | Occupied k v
  data HashMap k v = HM { buckets :: Vector (Bucket k v), size :: Int }

  emptyH :: HashMap k v
  lookupH :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
  insertH :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
  deleteH :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
  ```

  ```haskell
  -- Bag как оболочка над HashMap
  newtype Bag a = Bag { getBag :: HashMap a Int }

  emptyBag :: Bag a
  insertBag :: (Eq a, Hashable a) => a -> Bag a -> Bag a
  countBag  :: (Eq a, Hashable a) => a -> Bag a -> Int

  instance Semigroup (Bag a) where
    (<>) = mappendM

  instance Monoid (Bag a) where
    mempty = memptyM
  ```

  ```haskell
  -- Хэшируемый тип
  class Hashable k where
    hash :: k -> Int

  instance Hashable Int where
    hash = id
  ```

  ```haskell
  -- Пример использования
  let b = insertBag 'a' $ insertBag 'b' $ insertBag 'a' emptyBag
  countBag 'a' b  -- -> 2
  ```
  

**Тесты, отчёт инструмента тестирования, метрики**

- Unit-тесты (HUnit):
  - `test/Unit/BagSpec.hs` — тесты на вставку, удаление, map, filter, моноид, сравнение (6 тестов).
  - `test/Unit/HashMapSpec.hs` — тесты на insert/lookup, delete, foldl/foldr, map, filter, коллизии (6 тестов).

- Property-based тесты (QuickCheck):
  - `test/Properties/BagProp.hs` — свойства:
    1) `prop_insert_increases_count` — вставка увеличивает счётчик;
    2) `prop_totalCount_preserved_by_map` — map не меняет суммарный счёт элементов;
    3) `prop_monoid_identity` — нейтральный элемент для `mappendM` (локально);
    4) `prop_monoid_associative` — ассоциативность `mappendM` (локально);
    5) `prop_monoid_identity_std` — нейтральный элемент для `Monoid` (`<>`);
    6) `prop_monoid_associative_std` — ассоциативность `<>`.
  - `test/Properties/HashMapProp.hs` — свойства:
    1) `prop_insert_lookup` — insert затем lookup возвращает вставленное значение;
    2) `prop_map_values` — mapH корректно преобразует значения.

- Интеграция тестов:
  - Точка входа тест-сьюта: `test/Spec.hs`, который агрегирует unit и property тесты и запускается через `stack test`.

- Выводы инструментов (локальный прогон):
  - `stack test` (последний прогон) — собраны и выполнены все тесты: всего 23 теста, все пройдены.
  - Форматирование: `ormolu` запущен по `src` и `test` — код приведён к единому стилю.
  - CI (GitHub Actions): workflow обновлён — CI выполняет `stack test`, `hlint` и `ormolu --mode check`.

- Метрики качества впечатлений:
  - Количество unit-тестов: 12
  - Количество property-тестов: 8
  - Общее количество тестов в прогоне: 23 — все успешно пройдены.
  - Предупреждений компилятора после правок: минимальные; проект прогнан через `ormolu` и предупреждения по неиспользуемым импортам убраны.

**Выводы (оценка использованных приёмов программирования)**

- Функциональный стиль: все структуры реализованы как неизменяемые значения, операции возвращают новые структуры — соответствует парадигме Haskell.
- Полиморфизм и классы: использованы полиморфные интерфейсы (`Hashmap`, `BagClass`), что упрощает тестирование и потенциальную замену реализации.
- Безопасность API: скрытие `Internal` модулей в cabal гарантирует, что пользователи видят только чистый API и не зависят от внутренней реализации.
- Тестирование: сочетание HUnit и QuickCheck даёт хорошую гарантию корректности: HUnit проверяет конкретные сценарии, QuickCheck — общие свойства и законы (включая законы моноида).
- Производительность: HashMap реализован с открытой адресацией и вектором, операции ожидаемо O(1) в среднем; сравнение Bag через размеры + lookup обеспечивает эффективную операцию `==` без дорогостоящих преобразований.

TODO:

- Добавить измерения покрытия кода (HPC) и добавить тесты для неохваченных ветвей.
- Стресс-тестирование HashMap на больших объёмах данных для оценки скорости и поведения при переполнении (реализация сейчас использует фиксированную `initialCapacity`).
- Расширить properties (например, свойства для `delete`, инварианты `size`, свойства для `filter` и других краевых случаев).
- Подумать о реализации рехэширования (resize) для HashMap при достижении высокой загрузки.

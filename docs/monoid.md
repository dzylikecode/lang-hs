# Monoid

It seems that both `*` together with `1` and `++` along with `[]` share some common properties:

- The function takes two parameters.
- The parameters and the returned value have the same type.
- There exists such a value that doesn't change other values when used with the binary function.

---

`*` is associative, and so is `++`, but `-`, for example, is not. The expressions `(5 - 3) - 4` and `5 - (3 - 4)` result in different numbers.

---

A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function.

## interface

```hs
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

## law

```hs
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

## example

### List

```hs
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

### Product

```hs
newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)
```

### Sum

```hs
newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)
```

### All

```hs
newtype All = All {getAll :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)
```

### Any

```hs
newtype Any = Any {getAny :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)
```

### Ordering

```hs
instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT
```

---

```hs
lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  let a = length x `compare` length y
      b = x `compare` y
   in if a == EQ then b else a
```

```hs
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  (length x `compare` length y)
    `mappend` (x `compare` y)
```

和 compare 针对 string 的策略相同, 表现了 ordering 的 monoid 实例

归结来, 是 ordering

The `Ordering` monoid is very cool because it allows us to easily compare things by many different criteria and put those criteria in an order themselves, ranging from the most important to the least.

## Maybe

```hs
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

Notice the class constraint. It says that `Maybe a` is an instance of `Monoid` only if a is an instance of `Monoid`.

---

```hs
newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x
```

类似于 ordering 一样, 成为 monoid

```hs
getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- Just 9
```

---

类似的, 有 last

```hs
getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
-- Just 10
```

## fold

Using monoids to fold data structures

---

Much like Functor is for things that can be mapped over, Foldable is for things that can be folded up!

---

```hs
instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) =
    F.foldMap f l
      `mappend` f x
      `mappend` F.foldMap f r
```

通过 f 来确定 mempty 的类型

```hs
getAny $ F.foldMap (\x -> Any $ x > 15) testTree
```

## reference

- [A monad is just a monoid in the category of endofunctors, what's the problem?](https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem)

  ?> A monad is just a monoid in the category of endofunctors

# Applicative functor

对多元函数和 curry 宽泛的思考

```hs
fmap (*) (Just 3)
-- Just ((*) 3)
```

这只能对单元函数进行操作, 产生了 partially function, 然而无法进一步使用

希望能像下面一样使用多元, 对待 functor

```hs
f x y
```

we're looking for a more general and abstract way of doing that, which works across functors.

---

```hs
fmap (\f -> f 9) $ Just (*3)
-- Just 27
```

有点麻烦

---

`<*>` is left-associative, which means that `pure (+) <*> Just 3 <*> Just 5` is the same as `(pure (+) <*> Just 3) <*> Just 5`.

---

`pure f <*> x` equals `fmap f x`

---

`f <$> x <*> y <*> z` 类似于 `f x y z`

## interface

```hs
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

`pure` should take a value of any type and return an applicative functor with that value inside it. When we say inside it, we're using the box analogy again, even though we've seen that it doesn't always stand up to scrutiny.

`<*>`: Whereas `fmap` takes a function and a functor and applies the function inside the functor, `<*>` takes a functor that has a function in it and another functor and sort of extracts that function from the first functor and then maps it over the second one. When I say _extract_, I actually sort of mean _run_ and then extract, maybe even _sequence_.

## example

### Maybe

```hs
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
```

```hs
Just (*3) <*> Just 9
-- Just 27
```

### List

```hs
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
```

与下面相同

```hs
instance Applicative [] where
  pure x = [x]
  fs <*> xs = fmap (\f -> fmap (f) xs) fs
```

?> 暗含了`[]`的映射为`[]`

---

We apply every possible function from the left list to every possible value from the right list. The resulting list has every possible combination of applying a function from the left list to a value in the right one.

将 list 看作概率去理解

You can view lists as non-deterministic computations. A value like `100` or `"what"` can be viewed as a deterministic computation that has only one result, whereas a list like `[1,2,3]` can be viewed as a computation that can't decide on which result it wants to have, so it presents us with all of the possible results.

---

```hs
[ x*y | x <- [2,5,10], y <- [8,10,11]]
-- equals
(*) <$> [2,5,10] <*> [8,10,11]
```

### IO

```hs
instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)
```

it makes sense that `pure` is just `return`

---

```hs
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

### function

```hs
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)
```

1. exact `f` from functor: `f x`
2. exact `g` from functor: `g x`
3. apply `f x` to `g x`: `f x (g x)`
4. wrap `f x (g x)` to function: `\x -> f x (g x)`

---

```hs
(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- [8.0,10.0,2.5]
```

根据: `f <$> x <*> y <*> z` 类似于 `f x y z`

`x`看作`(r+3)`，`y`看作`(r*2)`，`z`看作`(r/2)`, 作为参数传递给`f`

第一步 partially:

```hs
\r -> (\y z -> [r+3,y,z]) <*> (*2) <*> (/2) $ 5
```

第二步 partially:

```hs
\r -> (\z -> [r+3,r*2,z]) <*> (/2) $ 5
```

第三步 partially:

```hs
\r -> [r+3,r*2,r/2]
```

### ZipList

> It turns out there are actually more ways for lists to be applicative functors.

一种是概率, 一种是向量的乘积

```hs
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

`pure` is also interesting here. It takes a value and puts it in a list that just has that value repeating indefinitely.

## lift

```hs
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```

With ordinary functors, we can just map functions over one functor. But with applicative functors, we can apply a function between several functors.

> 对多元函数的进一步思考

`(a -> b -> c) -> (f a -> f b -> f c)`

## sequence

```hs
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

> so elegant the definition of `sequenceA` is with the use of `pure` and `<*>`

It will transform a list of applicatives into an applicative with a list.

---

?> sequence 可以看作对多元为无穷的思考

`: 3 2 4 5` 变成了 `[3, 2, 4, 5]`

`: functor1 functor2 functor3` 类似的

---

fold

```hs
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
```

---

```hs
sequenceA [(+3),(+2),(+1)] 3
-- [6,5,4]
```

`[(\x -> x+3),(\x -> x+2),(\x -> x+1)]` 变成了 `(\x -> [x+3,x+2,x+1])`

`\x ->` 类似于 `Just`

---

Using `sequenceA` is cool when we have a list of functions and we want to feed the same input to all of them and then view the list of results. For instance, we have a number and we're wondering whether it satisfies all of the predicates in a list.

```hs
and $ map (\f -> f 7) [(>4),(<10),odd]
-- equals
and $ sequenceA [(>4),(<10),odd] 7
```

---

```hs
sequenceA [[1,2],[3,4],[5,6]]
-- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
```

## law

```hs
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
```

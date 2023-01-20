# Applicative functor

```hs
fmap (*) (Just 3)
-- Just ((*) 3)
fmap (\f -> f 9) $ Just (*3)
-- Just 27
```

第二种方式有点麻烦

we're looking for a more general and abstract way of doing that, which works across functors.

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

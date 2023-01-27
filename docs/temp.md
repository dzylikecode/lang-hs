```hs
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero
```

```hs
foldl (+) 0 (1:2:3:[])
    == foldl (+) (0 + 1)             (2:3:[])
    == foldl (+) ((0 + 1) + 2)       (3:[])
    == foldl (+) (((0 + 1) + 2) + 3) []
    ==           (((0 + 1) + 2) + 3)
```

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _    zero []     = zero
```

```hs
foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))
```

> 右结合不是需要用`(init xs x)`来代替`(x:xs)`, 而是考虑`step`的计算顺序

- left: `foldl step (step zero x) xs` 先计算`zero`与`x`
- right: `step x (foldr step zero xs)`, 最后计算`acc`与`x`

The class of functions that we can express using `foldr` is called **primitive recursive**.

```hs
myMap :: (a -> b) -> [a] -> [b]

myMap f xs = foldr step [] xs
    where step x ys = f x : ys
```

```hs
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
```

> 挺有意思的

# recursion

Recursion is actually a way of defining functions in which the function is applied inside its own definition.

---

Recursion is important to Haskell because unlike imperative languages, you do computations in Haskell by declaring what something is instead of declaring how you get it. That's why there are no while loops or for loops in Haskell and instead we many times have to use recursion to declare what something is.

## edge condition

Having an element or two in a recursion definition defined non-recursively (like _F(0)_ and _F(1)_ here) is also called the _edge condition_ and is important if you want your recursive function to terminate.

## infinite recursion

Because Haskell supports infinite lists, our recursion doesn't really have to have an edge condition.

```hs
repeat' :: a -> [a]
repeat' x = x:repeat' x
```

```hs
take 3 (repeat' 5)
-- [5,5,5]
```

由于 Haskell 的 lazy 性质, take 会终结`repeat'`的运算

## thinking recursively

We did quite a bit of recursion so far and as you've probably noticed, there's a pattern here. Usually you define an edge case and then you define a function that does something between some element and the function applied to the rest.

So when trying to think of a recursive way to solve a problem, try to think of when a recursive solution doesn't apply and see if you can use that as an edge case, think about identities and think about whether you'll break apart the parameters of the function (for instance, lists are usually broken into a head and a tail via pattern matching) and on which part you'll use the recursive call.

## fold

Usually, we'd have an edge case for the empty list. We'd introduce the `x:xs` pattern and then we'd do some action that involves a single element and the rest of the list. It turns out this is a very common pattern, so a couple of very useful functions were introduced to encapsulate it. These functions are called folds.

> fold 是对一种常见模式的抽象, 就像卷起地毯一样, 很形象

---

The binary function is called with the accumulator and the first (or last) element and produces a new accumulator. Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on.

> 产生一种迭代, 递归的结构 `the binary function`

- `foldl`

  `f 3 (f 4 (f 5 (f 6 z)))`

  可以看到 f 的形式为`\x acc -> ...`

- `foldr`

  `g (g (g (g z 3) 4) 5) 6`

  可以看到 g 的形式为`\acc x -> ...`

---

One big difference is that right folds work on infinite lists, whereas left ones don't! To put it plainly, if you take an infinite list at some point and you fold it up from the right, you'll eventually reach the beginning of the list. However, if you take an infinite list at a point and you try to fold it up from the left, you'll never reach an end!

---

`foldl1` and `foldr1` functions assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.

---

`scanl` and `scanr` are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list.

---

`a ++ (b ++ (c ++ (d ++ (e ++ f))))` 比下面更有效

`((((a ++ b) ++ c) ++ d) ++ e) ++ f`

```hs
import Control.Monad.Writer

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      return result
```

属于第二种, 如何变成第一种

```hs
import Control.Monad.Writer

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result <- gcd' b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result
```

> `fromDiffList`的概念非常有意思

无论 `a ++ (b ++ (c ++ (d ++ (e ++ f))))` 还是 `((((a ++ b) ++ c) ++ d) ++ e) ++ f` 都可以抽象为`f (g (h (i (j id))))`

- `a ++ (b ++ (c ++ (d ++ (e ++ f))))`

  `f(g x)` 为 `a ++ rest`

- `((((a ++ b) ++ c) ++ d) ++ e) ++ f`

  `f(g x)` 为 `rest ++ a`

用`id`作为初始值

## loop

the way to emulate a loop is via tail recursion

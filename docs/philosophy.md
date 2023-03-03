# philosophy

We defer every computation until its result is actually needed: Haskell is a lazy language. Laziness is not merely a matter of moving work around: it profoundly affects how we write programs.

---

It's important to recognise that each of these approaches involves tradeoffs. Very briefly put, the Haskell perspective emphasises safety, while the dynamically typed outlook favours flexibility.

---

类型似乎是一种限制, 相对于 js, py 的 any, 然后事实上 js, py 相当于把所有的东西都限制为一种类型 any

---

Haskell 在不断用函数丰富自己的词汇

距离 quickSort

首先, 自己调用自己

然后是, 使用 list comphrehension

最后有了 partition 词汇

```hs
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted
```

```hs
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smallerSorted = quickSort (filter (<= x) xs)
      biggerSorted = quickSort (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted
```

```hs
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let (smaller, bigger) = partition (<= x) xs
   in quickSort smaller ++ [x] ++ quickSort bigger
```

---

```hs
dzScanl :: (a -> a -> a) -> a -> [a] -> [a]
dzScanl f zero xs = foldl step [zero] xs
  where
    step acc x =
      let prevAcc = last acc
       in acc ++ [f prevAcc x]
```

想到 meta programming, decorate 模式, step 伪装成 f 被 foldl 调用, f 经过包装成 step 进行修改环境(context). 由此, 又想到 monad

---

For instance, when we were making a binary search tree, we didn't insert an element into a tree by modifying some tree in place. Our function for inserting into a binary search tree actually returned a new tree, because it can't change the old one.

---

多态也是抽象的表现

---

All languages have procedures, functions, and pieces of code that might fail in some way. That's just a fact of life.

---

Haskell's combination of purity, higher order functions, parameterized algebraic data types, and typeclasses allows us to implement polymorphism on a much higher level than possible in other languages. We don't have to think about types belonging to a big hierarchy of types.

Typeclasses are open, which means that we can define our own data type, think about what it can act like and connect it with the typeclasses that define its behaviors. Because of that and because of Haskell's great type system that allows us to know a lot about a function just by knowing its type declaration, we can define typeclasses that define behavior that's very general and abstract.

发现 polymorphism 和 functor 是很像的

```hs
fmap:: (a -> b) -> f a -> f b
```

只需要定义`a -> b`, 对`f a -> f b`都适用

---

monad, 各司其职, 对 IO, 就是对 IO 进行操作, 把外界看作一个思考的对象, 而不与其他 int 等混合在一起

---

将 side effect 抽象为 monad, 使得核心代码更为集中, 更为抽象通用, 易于扩展

```ghci
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
9 is too large, throwing it away
Keeping 1
5 is too large, throwing it away
Keeping 2
10 is too large, throwing it away
Keeping 3
```

---

laziness 适合作合理的假设

`thunk`, 使用 substitute 去理解

---

```c
int as_int(char *str)
{
  int acc; /* accumulate the partial result */

  for (acc = 0; isdigit(*str); str++) {
    acc = acc * 10 + (*str - '0');
  }

  return acc;
}
```

```hs
loop :: Int -> String -> Int

asInt xs = loop 0 xs
```

Rather than leap into blazing code, let's think about the data we have to work with.

将每个部分划分为函数进行思考

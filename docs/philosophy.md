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

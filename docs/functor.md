# functor

```hs
instance Functor [] where
    fmap = map
```

`[]` is a type constructor that takes one type and can produce types

What happens when we `map` or `fmap` over an empty list? Well, of course, we get an empty list. It just turns an empty list of type `[a]` into an empty list of type `[b]`.

> `empty`有点意思, 展现了多态

---

```hs
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

---

```hs
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
```

---

```hs
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

The `Functor` typeclass wants a type constructor that takes only one type parameter but `Either` takes two.

we'll partially apply `Either` by feeding it only one parameter so that it has one free parameter.

In the case of `Map k v`, `fmap` will map a function `v -> v'` over a map of type `Map k v` and return a map of type `Map k v'`.

---

A more correct term for what a functor is would be _computational context_.

---

```hs
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

---

The function type `r -> a` can be rewritten as `(->) r a`

When we look at it as `(->) r a`, we can see `(->)` in a slighty different light, because we see that it's just a type constructor that takes two type parameters, just like `Either`.

That's why we can't make `(->)` an instance of `Functor`, but if we partially apply it to `(->) r`, it doesn't pose any problems.

```hs
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
```

类比 IO 的 functor (the box analogy)

`result <- action` is the same as `g x`: 取出

`return f` is similar to `\x -> f x` : 作用并包装入箱子

所以 `return (f result) = (\x -> f (g x))`

```hs
\x -> f (g x) = \x -> f.g $ x = f.g
```

Function composition!

```hs
instance Functor ((->) r) where
    fmap = (.)
```

---

观察 type signature

`fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`

`fmap :: (a -> b) -> (r -> a) -> (r -> b)`

也说明了 Function composition!

---

`f a -> f b` f 看作是容器, a 看作是容器里面的东西

`f a` 构造, 即放入容器中, 采用 r 进行包装`-> r a`(包装在了函数里面), 即`\r -> a`

`f a` 解构, 即取出容器, `f x`进行函数计算, 即取出了`a`

---

`fmap :: (a -> b) -> (f a -> f b)`

It takes an `a -> b` function and returns a function `f a -> f b`(it takes a functor as a parameter and returns a functor as the result.). This is called _lifting_ a function.

这类似于多态, 应用于某个范畴, 类似的会得到另一个范畴的东西

---

```hs
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

## law

```hs
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)
```

If a type obeys the functor laws, we know that calling `fmap` on a value of that type will only map the function over it, nothing more. This leads to code that is more abstract and extensible, because we can use laws to reason about behaviors that any functor should have and make functions that operate reliably on any functor.

## reference

Functor (functional programming). (2022, December 22). In Wikipedia. https://en.wikipedia.org/wiki/Functor_(functional_programming)

Functor. (2022, December 25). In Wikipedia. https://en.wikipedia.org/wiki/Functor

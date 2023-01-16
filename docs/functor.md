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

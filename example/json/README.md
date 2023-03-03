# json

## Princeton

source: https://www.cs.princeton.edu/~dpw/cos441-11/notes/lec07-type-classes.html

[](./princeton/data.hs ":include :type=code hs")

名字非常繁琐, 尤其是`xsToJSON`与`xysToJSON`还要传递`f`进去

```hs
xysToJSON stringToJSON [("day", "monday"), ("loc", "zanzibar")]
-- JObj [("day",JStr "monday"),("loc",JStr "zanzibar")]
```

> 希望能像函数重载一样

!> Haskell 不能 overloading function

- [Overloading function signatures haskell](https://stackoverflow.com/questions/6119225/overloading-function-signatures-haskell)

  使用 typeclass

[](./princeton/typeclass.hs ":include :type=code hs")

`class JSON`与 `data JVal`很相似, 都有 BNF notation 的感觉

---

由于 Haskell 的 list 具有 homogeneous 的特性, 不能充分对于 json

```hs
toJSON ["cat", "dog", "Mouse"]
```

只能类似上面

tuple 可以不同质, 但是 instance 化很麻烦

[](./princeton/typeclassEx.hs ":include :type=code hs")

```hs
hs =
  ( ("name", "Ranjit"),
    ("age", 33 :: Double),
    ("likes", ["guacamole", "coffee", "bacon"]),
    ("hates", ["waiting", "grapefruit"]),
    ("lunches", lunches)
  )
```

就像下面代码不能通过

```hs
(1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,1) == (1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,1)
```

因为 Haskell 没有 instance 这么长的 tuple

# type

根据类型和函数的名字, 这些信息, 可以大概猜测函数的功能

---

查阅类型

```bash
$ ghci
> :t zip # :type zip
```

---

type signature

`::`is read as "has type of".

```hs
'a' :: Char
-- 'a' has type of Char
double :: Int -> Int
double x = 2 * x
-- `double` has type of `Int -> Int`
(True, 'a') :: (Bool, Char)
```

## type variable

a type variable is not in capital case

必须小写

---

类似于 any, 会匹配任何类型

```hs
func:: a -> b
```

`func`接受任何类型的参数(a)，返回任何类型的值(b, b 可以与 a 的类型相同)

```hs
func:: a -> a
```

`func`接受任何类型的参数(a)，返回与 a 相同类型的值(a)

---

针对 algebraic data type, (类似其他 language 的 class)

```hs
fst:: (a, b) -> a
```

`fst`接受(a, b)类型的参数，返回与 a 相同类型的值

`(,)`相当于是构造器

`(a, b)` 相当于将 tuple 析构为两个子类型 a, b

像 pattern matching, 析构

```hs
head:: [a] -> a
```

`head`接受[a]类型的参数，返回与 a 相同类型的值

`[]`相当于是构造器

```hs
func:: Maybe a -> a
```

---

Functions that have type variables are called **polymorphic functions**.

## Typeclasses

A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes.

不是其他语言的 class, 而是 interface

比如 ts 的 interface

We explained that a typeclass is a sort of an interface that defines some behavior. A type can be made an instance of a typeclass if it supports that behavior. Example: the `Int` type is an instance of the `Eq` typeclass because the `Eq` typeclass defines behavior for stuff that can be equated.

Typeclasses are more like interfaces. We don't make data from typeclasses. Instead, we first make our data type and then we think about what it can act like.

---

```hs
(==) :: (Eq a) => a -> a -> Bool
```

Everything before the `=>` symbol is called a **class constraint**.

意思是 a 要满足 Eq 这个 interface

---

several class constraints

```hs
fromIntegral :: (Num b, Integral a) => a -> b
```

### example

`Eq`

---

`Ord`

**Ordering** is a type that can be `GT`, `LT` or `EQ`, meaning _greater than_, _lesser than_ and _equal_, respectively.

---

`show`: convert to string

```hs
show:: Show a => a -> String
```

---

`read`: convert from string

```hs
read:: Read a => String -> a
```

如果推导不出来, 就需要明示

```hs
read "3" + 5
-- 8
read "3":: Int
-- 3
read "3":: Float
-- 3.0
```

---

`Enum`: Num, Char

```hs
[LT .. GT]
```

---

`Num`

### deriving

Haskell can derive the behavior of our types in these contexts if we use the _deriving_ keyword when making our data type.

```hs
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)
```

## type parameters

A value constructor can take some values parameters and then produce a new value. For instance, the `Car` constructor takes three values and produces a car value. In a similar manner, **type constructors** can take types as parameters to produce new types.

```hs
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

对类型进一步抽象

```hs
data Shape a = Circle a Float | Rectangle a a deriving (Show)
```

此时`Shape Point`相当于原来的`Shape`

---

Notice that the type of `Nothing` is `Maybe a`. Its type is polymorphic.

just like `5` can act like an `Int` or a `Double`.

---

Using type parameters is very beneficial, but only when using them makes sense.

```hs
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

!> 过度的抽象

```hs
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)
tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

We'd have to force this function to take a `Car` type of `(Show a) => Car String String a`.

---

Having maps parameterized enables us to have mappings from any type to any other type, as long as the type of the key is part of the `Ord` typeclass. If we were defining a mapping type, we could add a typeclass constraint in the _data_ declaration:

```hs
data (Ord k) => Map k v = ...
```

However, it's a very strong convention in Haskell to **never add typeclass constraints in data declarations**.

because we don't benefit a lot, but we end up writing more class constraints, even when we don't need them.

If we put or don't put the `Ord k` constraint in the data declaration for `Map k v`, we're going to have to put the constraint into functions that assume the keys in a map can be ordered. But if we don't put the constraint in the data declaration, we don't have to put `(Ord k) =>` in the type declarations of functions that don't care whether the keys can be ordered or not. An example of such a function is `toList`, that just takes a mapping and converts it to an associative list. Its type signature is `toList :: Map k a -> [(k, a)]`. If `Map k v` had a type constraint in its data declaration, the type for `toList` would have to be `toList :: (Ord k) => Map k a -> [(k, a)]`, even though the function doesn't do any comparing of keys by order.

> 让函数去约束

## type synonyms

they're just about giving some types different names so that they make more sense to someone reading our code and documentation.

```hs
type String = [Char]
type PhoneBook = [(String,String)]
phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
```

---

Type synonyms can also be parameterized.

```hs
type AssocList k v = [(k,v)]
```

### partially

Just like we can partially apply functions to get new functions, we can partially apply type parameters and get new type constructors from them.

```hs
type IntMap = Map Int
-- equals
type IntMap v = Map Int v
```

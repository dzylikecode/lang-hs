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

## example

### Char

`'a'`

### Bool

|  C   | Haskell |
| :--: | :-----: |
| `!=` |  `/=`   |
| `!`  |  `not`  |

### list

`[a]`

---

a homogenous data structure

同质的数据结构

类似于 C++, 而不像 python, js

---

字符串也是 list, 只不过是 syntactic sugar

---

`++`: merge two list to one

```hs
[1, 2] ++ [3, 4]
-- [1, 2, 3, 4]
```

---

`:`: list constructor

```hs
1 : [2, 3]
-- [1, 2, 3]
```

```hs
[1, 2, 3]
-- syntatic sugar for
-- 1:2:3:[]
```

---

`!!`: index to get element

start at 0

```hs
[11, 3, 2] !! 1
-- 3
```

---

compare

- `>=`
- `>`
- `=`

类似于字符串之间的比较

---

head

tail

init

last

...

> 也许可以弄一个 API

---

```hs
[2,4..20]
-- equals
[2,4,6,8,10,12,14,16,18,20]
```

适用于等差数列

枚举的子类: number, char

!> 不要使用 float

---

infinite list

- circle
- repeat

由于 Haskell 的 lazy 性质

```hs
take 12 (cycle "LOL ")
-- "LOL LOL LOL "
```

---

形似:

$$\{x^2 | x \in \mathbb{N}, x \leq 100\}$$

```hs
[ x | x <- [50..100], x `mod` 7 == 3]
-- [52,59,66,73,80,87,94]
[ x*y | x <- [2,5,10], y <- [8,10,11]]
-- [16,20,22,40,50,55,80,100,110]
[ func x y | x <- ls1, y <- ls2, expBool_1 x y, expBool_2 x y]
```

!> 生成的 x, y 是每种组合都有

## tuple

fixed length

不必同质: They don't have to be homegenous

```hs
(3, "Hello")
```

---

不同长度就是不同类型

不同内部类型就是不同类型

特别的, `()`就是一种独一无二的类型

!> there's no such thing as a singleton tuple

`(a)` 仅仅就是计算一个表达式

---

`zip`

```hs
zip [1..] [5, 5, 5]
-- [(1,5),(2,5),(3,5)]
```

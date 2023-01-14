# data

## build in

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

### tuple

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

## algebraic data type

```hs
data Bool = False | True
```

`Bool` 是类型

`False`, `True`是构造函数(不接受任何参数)

---

可以用来做枚举

```hs
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
```

---

```hs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

类似于 C++

```cpp
class Shape {};

class Circle: public Shape {
public:
    Circle(float x, float y, float r): x(x), y(y), r(r) {}
private:
    float x, y, r;
};

class Rectangle: public Shape {
public:
    Rectangle(float x1, float y1, float x2, float y2): x1(x1), y1(y1), x2(x2), y2(y2) {}
private:
    float x1, y1, x2, y2;
};
```

用构造函数得到的都是 base class

!> 与 C++ 不同的是: `Circle` is not a type, `Shape` is.

It says that the function takes a shape and returns a float. We couldn't write a type declaration of `Circle -> Float` because `Circle` is not a type, `Shape` is. Just like we can't write a function with a type declaration of `True -> Int`.

---

we can pattern match against constructors.

```hs
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

一样可以解析构造

c++类似的不是这样

```cpp
float surface(const Circle&  c) { return 3.14 * c.r * c.r; }
float surface(const Rectangle& r) { return (r.x2 - r.x1) * (r.y2 - r.y1); }
int main() {
    Shape *s = &c;
    std::cout << "surface(Shape):" << surface(*s) << std::endl;
    // c++ 会报错
    // no instance of overloaded function "surface" matches the argument list
    return 0;
}
```

---

```hs
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
```

`fisrtName` 相当于如下, 不需要冗余地写

```hs
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname
```

---

```hs
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
-- assign
aCar = Car {company="Ford", model="Mustang", year=1967}
-- deconstruct
getCompany Car{company=com} = com
```

### nested type

```hs
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

we used the same name for the data type and the value constructor.

> data type 与 value constructor 可以相同, 也可以不同

---

解构

```hs
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

---

新的理解

```hs
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
```

We could also opt not to export any value constructors for `Shape` by just writing `Shape` in the export statement. That way, someone importing our module could only make shapes by using the auxilliary functions `baseCircle` and `baseRect`. `Data.Map` uses that approach.

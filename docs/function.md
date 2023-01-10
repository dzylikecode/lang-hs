# function

## 调用

用空格分开参数

```hs
func arg1 arg2 arg3
```

!> parentheses are unnecessary to denote function application. Don't confuse it with C notation.

```hs
bar (bar 3)
-- equals
x = bar 3
bar x
```

---

对于二元运算

- `prefex`

  ```hs
  func arg1 arg2
  ```

- `infex`

  ```hs
  arg1 `func` arg2
  ```

  ?> If a function is comprised only of special characters, it's considered an infix function by default. If we want to examine its type, pass it to another function or call it as a prefix function, we have to surround it in parentheses.

example:

```hs
2 + 3
-- the same as
(+) 2 3
-------------
in 2 [2, 3]
-- the infex style has better readibility
2 `in` [2, 3]
```

---

函数有最高的优先级

the highest precedence

```hs
succ 9 * 10
-- equals
(succ 9) * 10
```

## definition

```hs
-- definition
f x = x*x
-- application
f 3
-- 9
```

---

infix

```hs
x `myPlus` y = x + y
```

---

函数的先后顺序不重要

```hs
-- the order of the function application does not matter
-- call `square` before it's definition
length x y = sqrt (square x + square y)
square x = x * x
```

### name

类似于 C 语言, 特别的`'`也是合法字符

```hs
mark'book = "math"
```

采用 camel 风格

!> 首字母小写, 大写为类型

### All is expression

表达式形式

形如

$$f(x)= x^2$$

```hs
-- definition
f x = x*x
-- application
f 3
-- 9
```

---

if-else is also an expression

形如:

$$f(x) = \begin{cases}x^2 & x \geq 0 \\-x^2 & x < 0 \end{cases}$$

```hs
f x =
  if x >= 0
    then x ^ 2
    else -x ^ 2
```

!> 因此必须有 else

```hs
[if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
-- ["Woo","Bar"]
4 * (if 10 > 5 then 10 else 0) + 2
-- 42
```

## pattern matching

syntactic constructs

---

```hs
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
```

the patterns will be checked from top to bottom and when it conforms to a pattern, the corresponding function body will be used.

!> order matters

---

When defining functions, you can define separate function bodies for different patterns. This leads to really neat code that's simple and readable.

> 重构了 if-else tree, 将不同重点的逻辑分离出来

类比 js

```js
function sayMe(x) {
  if (x === 1) {
    return "One!";
  }
  if (x === 2) {
    return "Two!";
  }
  if (x === 3) {
    return "Three!";
  }
  if (x === 4) {
    return "Four!";
  }
  if (x === 5) {
    return "Five!";
  }
  return x;
}
```

觉得不如 Haskell 清晰

### deconstructing

析构成功与否, 意味着 pattern matching 成功与否

```hs
first:: (a, b, c) -> a
first (x, _, _) = x
```

值在传递的时候发生析构

```hs
first (1, 2, 3)
-- 1
first triple
--
let (_, x, _) = triple
```

---

对于 algebraic data type

```hs
getId (Book id _ _) = id
```

---

list 类似于 algebraic data type

```hs
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
```

`:`是它的一个构造函数

---

as pattern

```hs
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```

相当于值的传递过程为

$$arg \rightarrow all \rightarrow (x:xs)$$

## guards

Whereas patterns are a way of making sure a value conforms to some form and deconstructing it, guards are a way of testing whether some property of a value (or several of them) are true or false.

```hs
-- bmi: body mass index
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise                   = "You're a whale, congratulations!"
```

!> `bmiTell weight height`后面没有`=`

`otherwise` is a predefined function that always returns True. (build-in)

```hs
otherwise = True
```

---

If all the guards of a function evaluate to `False` (and we haven't provided an `otherwise` catch-all guard), evaluation falls through to the next `pattern`.

## where

Where bindings are a syntactic construct that let you bind to variables at the end of a function and the whole function can see them, including all the guards.

```hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi    = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0
```

---

_where_ bindings aren't shared across function bodies of different patterns. If you want several patterns of one function to access some shared name, you have to define it globally.

---

pattern matching

```hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi                   = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
```

the whole function can see them

```hs
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
```

---

_where_ bindings can also be nested. It's a common idiom to make a function and define some helper function in its _where_ clause and then to give those functions helper functions as well, each with its own _where_ clause.

---

the function body is closer to its name and type declaration and to some that's more readable.

## let

Let bindings let you bind to variables anywhere and are expressions themselves, but are very local, so they don't span across guards.

`let` bindings are expressions themselves

!> `let` don't span across guards

```hs
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
   in sideArea + 2 * topArea
```

---

一般用于定义概念, 取别名

```hs
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height =
  let bmi = weight / height ^ 2
   in bmiTell' bmi
  where
    bmiTell' index
      | index <= skinny = "You're underweight, you emo, you!"
      | index <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
      | index <= fat    = "You're fat! Lose some weight, fatty!"
      | otherwise       = "You're a whale, congratulations!"
      whereskinny = 18.5
        normal = 25.0
        fat    = 30.0
```

bmi 的含义不是很清楚, 所以先定义出来, 而 skinny, normal, fat 的含义很清楚, 所以往后定义

然而这样非常繁琐, 不如直接用 where 那么简洁, 相当于大概明白 bmi 的含义

---

The difference is that _let_ bindings are expressions themselves. _where_ bindings are just syntactic constructs.

```hs
4 * (let a = 9 in a + 1) + 2
-- 42
```

---

If we want to bind to several variables inline, we obviously can't align them at columns. That's why we can separate them with semicolons.

```hs
(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
-- (6000000,"Hey there!")
```

```hs
x =
  ( let a = 100
        b = 200
        c = 300
     in a * b * c,
    let foo = "Hey "; bar = "there!" in foo ++ bar
  )
-- (6000000,"Hey there!")
```

---

We include a _let_ inside a list comprehension much like we would a predicate, only it doesn't filter the list, it only binds to names. The names defined in a _let_ inside a list comprehension are visible to the output function (the part before the `|`) and all predicates and sections that come after of the binding.

```hs
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```

We can't use the `bmi` name in the `(w, h) <- xs` part because it's defined prior to the _let_ binding.

## case

Oh yeah, pattern matching on parameters in function definitions! Well, that's actually just syntactic sugar for case expressions. These two pieces of code do the same thing and are interchangeable:

```hs
head' :: [a] -> a
head' []    = error "No head for empty lists!"
head' (x:_) = x
```

```hs
head' :: [a] -> a
head' xs = case xs of
  []      -> error "No head for empty lists!"
  (x : _) -> x
```

---

Because pattern matching in function definitions is syntactic sugar for case expressions

```hs
describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    []  -> "empty."
    [x] -> "a singleton list."
    xs  -> "a longer list."
```

```hs
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what []  = "empty."
          what [x] = "a singleton list."
          what xs  = "a longer list."
```

## indent
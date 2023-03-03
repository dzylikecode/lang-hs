# json

source: https://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html

## data

[](jsonData.md ":include :type=code markmap")

## render

```hs
renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v
renderJValue (JArray a)    = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)
```

## pretty

```hs
-- Render.hs
renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
```

### string

When we must pretty print a string value, JSON has moderately involved escaping rules that we must follow. At the highest level, a string is just a series of characters wrapped in quotes.

?> 不断用函数转化思考的对象

通过以下转化, 变成重点思考 `oneChar`

```hs
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar
```

!> Haskell 采用的是 utf-8, 需要转化为 json 的 unicode 形式, material: https://javascript.info/unicode

- `encloase`

  ```hs
  enclose :: Char -> Char -> Doc -> Doc
  enclose left right x = char left <> x <> char right
  ```

- `hcat`: similar to `concat`

  ```hs
  hcat :: [Doc] -> Doc
  hcat xs = undefined
  ```

---

`oneChar`

通过以下转化, 变成重点思考 `hexEscape`

```hs
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
            Just r -> text r
            Nothing | mustEscape c -> hexEscape c
                    | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])
```

$$
\begin{cases}
  \setminus \text{b} &\rightarrow \setminus \setminus \text{b} \\
  \text{a} &\rightarrow \text{a} \\
  \text{佫} &\rightarrow \setminus \text{u \{ 20331 \} } \\
\end{cases}
$$

- `\b`: control characters
- `a`: printable ASCII characters
- `佫`: utf-8

---

由于 `hexEscape` 有两种情况

$$
\text{hexEscape}(x) =
\begin{cases}
    \text{smallHex}(x) &{\text{ if }} x \in [\text{0x0}, \text{0xffff}] \\
    \text{astral}(x-\text{0x10000}) &{\text{ if }} x \in [\text{0x10000}, \text{0x10ffff}] \\
\end{cases}
$$

```hs
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c
```

[`ord`(order)](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Char.html#v:ord): convert a character to an integer -- from `Data.Char`

---

`smallHex`

```hs
smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0') -- padding 0
           <> text h
    where h = showHex x ""
```

showHex: convert an integer to a hexadecimal string -- from `Numeric`

形成`\u%04x`的形式

---

`astral`

```hs
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff
```

!> 我就不仔细研究了 :hugs:

## array and object

抽象思考

```hs
-- render.hs
renderJValue (JArray ary) = series '[' ']' renderJValue ary

renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val
```

```hs
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
```

?> 有意思的是`punctuate (char ',')`, 是在`Doc`上面思考的

## doc

Observe that the Doc type is actually a tree. The Concat and Union constructors create an internal node from two other Doc values, while the Empty and other simple constructors build leaves.

[](docData.md ":include :type=code markmap")

---

The `Line` constructor represents a line break. The `line` function creates _hard_ line breaks, which always appear in the pretty printer's output. Sometimes we'll want a _soft_ line break, which is only used if a line is too wide to fit in a window or page. We'll introduce a `softline` function shortly.

考虑像下面一下, 随着 width 大小而换行

```hs
pretty :: Int -> Doc -> String
```

```ghci
ghci> putStrLn (pretty 10 value)
{"f": 1.0,
"q": true
}
ghci> putStrLn (pretty 20 value)
{"f": 1.0, "q": true
}
ghci> putStrLn (pretty 30 value)
{"f": 1.0, "q": true }
```

这是由`fsep`控制产生的, 插入了`softline`

```hs
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line
```

The `softline` function should insert a newline if the current line has become too wide, or a space otherwise. How can we do this if our Doc type doesn't contain any information about rendering? Our answer is that every time we encounter a soft newline, we maintain _two_ alternative representations of the document, using the `Union` constructor.

`softline`实际上就是个`Union`, 而`Union`就是针对`softline`而设计的

```hs
group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten Line           = Char ' '
```

`Union`的左边代表宽度足够的使用

```ghci
ghci> putStrLn (pretty 30 value)
{"f": 1.0, "q": true }
```

不够使用的时候, 会使用`Union`的右边

```ghci
ghci> putStrLn (pretty 20 value)
{"f": 1.0,
"q": true
}
```

### compact

compact 不需要考虑行宽, 一律采用换行

```ghci
ghci> putStrLn (compact value)
{"f": 1.0,
"q": true
}
```

---

```hs
compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) = case d of
          Empty        -> transform ds
          Char c       -> c : transform ds
          Text s       -> s ++ transform ds
          Line         -> '\n' : transform ds
          a `Concat` b -> transform (a:b:ds)
          _ `Union` b  -> transform (b:ds)
```

一律采用换行, 对于`Union`, 只需要采用右边的

> transform 实际上是将 Doc(即 Tree), 转化为 List(即 Stack)进行处理

```hs
a `Concat` b -> transform (a:b:ds)
```

将 x 分解为`a`和`b`, 然后压栈`a:b:ds`, `transform (a:b:ds)`是对 stack 的处理

### pretty

```hs
pretty width x = best 0 [x]
  where
    best col (d:ds) = case d of
      Empty        -> best col ds
      Char c       -> c :  best (col + 1) ds
      Text s       -> s ++ best (col + length s) ds
      Line         -> '\n' : best 0 ds
      a `Concat` b -> best col (a:b:ds)
      a `Union` b  -> nicest col (best col (a:ds))
                                 (best col (b:ds))
    best _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise                = b
      where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
```

`fits`考虑宽度是否合适

## summary

不断用函数转化思考的对象, example: [string](#string)

---

抽象思考, 如 string 看作`[char -> doc]`, `object`和`array`看作是相同的

`item`看作是`a -> doc`, 反过来`a -> doc`也可以看作是`item`

---

思考的时候可以自顶向下, 根据需求来设计

```hs
data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
```

## further

- [#Practical pointers and further reading](https://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html#id602458)

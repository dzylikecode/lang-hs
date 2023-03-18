# parse binary

We have given our parsing state a name. When we name something, it can become easier to reason about. For example, we can now look at parsing as a kind of function: it consumes a parsing state, and produces both a new parsing state and some other piece of information. We can directly represent this as a Haskell type.

```hs
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
```

很不错, 将我平时的 parseNumber, parseParagraph 抽象了. Parse a 是静态的, 被称为 parser, 而取出里面的 function, 确实是 runParse, 动态的

---

```hs
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
```

很有意思, 这是一个`identity`生成工厂, `identity 1` 就是 `Parse (\s -> Right (1, s))`, `identity "foo"` 就是 `Parse (\s -> Right ("foo", s))`

parser _generator_

---

```hs
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result
```

---

If we want to add more information to the parsing state, all we need to do is modify the definition of `ParseState`, and the bodies of whatever functions need the new information. Compared to our earlier parsing code, where all of our state was exposed through pattern matching, this is much more modular: the only code we affect is code that needs the new information.

---

```hs
(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
```

可以对比 monad 的 state 实现

同样`secondParser`是一个`parser generator`

---

```hs
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)
```

---

```hs
ghci> parse (id <$> parseByte) input
Right 102
```

---

```hs
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState
```

阅读[整体的代码](https://github.com/bendoerr/real-world-haskell/blob/master/ch10/Parse.hs)可以知道, string 是取出 state 的 string 部分

`fmap fst`是映射到`Maybe`的

---

```hs
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []
```

---

```hs
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")
```

真的优雅, 用 monad 更加清晰

help functions:

```hs
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
```

## References

1. [Chapter 10. Code case study: parsing a binary data format](https://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html)

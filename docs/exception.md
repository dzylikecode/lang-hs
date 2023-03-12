# exception

`Maybe` and `Either`可以用来表示错误

Despite having expressive types that support failed computations, Haskell still has support for exceptions, because they make more sense in I/O contexts. A lot of things can go wrong when dealing with the outside world because it is so unreliable.

---

Pure code can throw exceptions, but it they can only be caught in the I/O part of our code (when we're inside a do block that goes into `main`). That's because you don't know when (or if) anything will be evaluated in pure code, because it is lazy and doesn't have a well-defined order of execution, whereas I/O code does.

---

The logic of our program should reside mostly within our pure functions, because their results are dependant only on the parameters that the functions are called with. When dealing with pure functions, you only have to think about what a function returns, because it can't do anything else. This makes your life easier. Even though doing some logic in I/O is necessary (like opening files and the like), it should preferably be kept to a minimum. Pure functions are lazy by default, which means that we don't know when they will be evaluated and that it really shouldn't matter. However, once pure functions start throwing exceptions, it matters when they are evaluated. That's why we can only catch exceptions thrown from pure functions in the I/O part of our code. And that's bad, because we want to keep the I/O part as small as possible. However, if we don't catch them in the I/O part of our code, our program crashes. The solution? Don't mix exceptions and pure code. Take advantage of Haskell's powerful type system and use types like `Either` and `Maybe` to represent results that may have failed.

---

exception 鼓励先实现，不要害怕错误，犯了错误直接丢出来然后解决，而不是一开始考虑所有的错误

## catch

function from `System.IO.Error`

```hs
catch :: IO a -> (IOError -> IO a) -> IO a
```

`IOError`: which is a value that signifies that an I/O exception occurred.

由系统封装的, we can't inspect values of the type IOError by pattern matching against them, but we can use a bunch of useful predicates to find out stuff about values of type `IOError`

---

```hs
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
```

we re-throw the exception that was passed by the handler with the `ioError` function. It has a type of `ioError :: IOException -> IO a`, so it takes an `IOError` and produces an I/O action that will throw it. The I/O action has a type of `IO a`, because it never actually yields a result, so it can act as `IO anything`.

## handle

```hs
saferFileSize path = handle (\_ -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
```

## with

```hs
getFileSize path = handle (\_ -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
```

Is the order in which we call `bracket` and `handle` important? Why?

yes, 调换顺序是不可以的, 因为需要先打开 file, 才能读取 file size

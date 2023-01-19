# Input and Output

It turns out that Haskell actually has a really clever system for dealing with functions that have side-effects that neatly separates the part of our program that is pure and the part of our program that is impure, which does all the dirty work like talking to the keyboard and the screen. With those two parts separated, we can still reason about our pure program and take advantage of all the things that purity offers, like laziness, robustness and modularity while efficiently communicating with the outside world.

## do

we can use do syntax to glue together several I/O actions into one.

```hs
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

By putting them together with _do_ syntax, we glued them into one I/O action. The action that we got has a type of `IO ()`, because that's the type of the last I/O action inside.

---

Because of that, `main` always has a type signature of `main :: IO something`, where `something` is some concrete type. By convention, we don't usually specify a type declaration for `main`.

---

If we want to deal with impure data, we have to do it in an impure environment. So the taint of impurity spreads around much like the undead scourge and it's in our best interest to keep the I/O parts of our code as small as possible.

`name <- getLine `name 代表的是 pure, `getLine` 代表的是 impure. 通过这行代码分离了 pure 和 impure. 依赖`name`的代码, 可以是 pure code

---

Every I/O action that gets performed has a result encapsulated within it.

```hs
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

Except for the last line, every line in a do block that doesn't bind can also be written with a bind. So `putStrLn "BLAH"` can be written as `_ <- putStrLn "BLAH"`.

!> Except for the last line

**the last action** cannot be bound to a name like the first two were.

需要使用`return`才行

```hs
main = do
    foo <- putStrLn "Hello, what's your name?"
    return foo
```

---

```hs
name = getLine
```

all this does is give the `getLine` I/O action a different name called

---

`do`是一个有副作用的表达式, 一样有返回值

you can think of it in the way that the do block automatically extracts the value from the last action and binds it to its own result.

---

`do`是一个表达式

```hs
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

也可以添加一个 `do`

```hs
main = do
    line <- getLine
    if null line
        then do return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

---

I/O actions will only be performed when they are given a name of `main` or when they're inside a bigger I/O action that we composed with a do block.

---

就像 List Comprehension 一样, `let` 可以不需要 `in`

```hs
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```

## return

`return` is a function that takes a value and puts it in a minimal context. In the case of `IO`, it means wrapping it in an I/O action.

!> 它不会终止程序

```hs
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line
```

All these `return`s do is that they make I/O actions that don't really do anything except have an encapsulated result and that result is thrown away because it isn't bound to a name.

---

```hs
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
```

上下效果相同

```hs
main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b
```

## 构造语言

`when`

it's useful for encapsulating the `if something then do some I/O action else return ()` pattern.

```hs
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
```

---

`sequence :: [IO a] -> IO [a]`

```hs
map print [1,2,3,4]
-- [print 1, print 2, print 3, print 4]
sequence $ map print [1,2,3,4]
-- 1
-- 2
-- 3
-- 4
-- IO [(),(),(),()]
```

> 1, 2, 3, 4 是副作用, `IO [(),(),(),()]`是结果

---

`mapM` and `mapM_`

```hs
mapM f = sequence . map f
mapM_ f xs = do
    sequence_ . map f $ xs
    return ()
```

---

类似迭代器

```hs
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
```

`forM`有返回值, 类似于`map`

---

This pattern of getting some string from the input, transforming it with a function and then outputting that is so common that there exists a function which makes that even easier, called `interact`.

把某个模式分离出来, 更加关注自己所关心的抽象

---

类似的, encapsulated `withFile`

```hs
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
```

## command line arguments

That's called an interactive program and the difficult bit with interactive command line programs is this — what if you want to automate the execution of that program, like with a batch script? It's harder to make a batch script that interacts with a program than a batch script that just calls one program or several of them.

如同有副作用, 写成函数的形式更好

That's why it's sometimes better to have the user tell the program what they want when they run the program, instead of having the program ask the user once it's run.

---

```hs
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
```

we made a dispatch association that maps from commands to functions that take some command line arguments and return an I/O action.

## randomness

```hs
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
```

多态, 确定类型

---

通过多次迭代来产生随机数

```hs
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)
```

同样的`gen`代入`random`生成的结果是一样

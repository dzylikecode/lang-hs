# search file

- source: https://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html

Individual expressions can take such forms as “name matches this glob pattern”, “entry is a plain file”, “last modified before this date”, and many more. They can be stitched together into more complex expressions using “and” and “or” operators.

> 有的时候, 可以用 predication 去思考被人说的话

```hs
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
```

## find

```hs
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)
```

- the predicate is not very expressive. It can only look at the name of a directory entry; it cannot, for example, find out whether it's a file or a directory. This means that our attempt to use `simpleFind` will list directories ending in `.c` as well as files with the same extension.
- `simpleFind` gives us no control over how it traverses the filesystem.
- `simpleFind` is strict

## Predicates

from poverty to riches, while remaining pure

```hs
type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> ClockTime     -- last modified
               -> Bool
```

认识到文件可以由四元组抽象, 而 predicates 其实是四元组到 bool 的映射

---

变形

```hs
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
      perms    <- getPermissions name
      size     <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)
```

## DSL

Our predicate will check for a C++ source file that is over 128KB in size.

```hs
myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False
```

可以分解为两个 predicates

---

more concise predicates

```hs
pathP path _ _ _ = path
```

将文件(四元组)映射到熟悉的范畴进行思考

```hs
type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> ClockTime       -- last modified
             -> a
```

于是, pathP 可以看作文件范畴的 path

```hs
pathP :: InfoP FilePath
```

类似的有 size

```hs
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1
```

更进一步

a quick glance shows that the `Predicate` type that we defined near the beginning of this chapter is the same type as InfoP Bool. (We could thus legitimately get rid of the `Predicate` type.)

于是观察`myTest`函数是根据文件范畴的路径和大小, 得到一个文件范畴的 bool

---

定义这个范畴的比较

```hs
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k
```

> 这里面有范畴的交互, monad

文件范畴的 path 和普通范畴的 path 进行比较得到文件范畴的 bool

curry 的缘故

```hs
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k
```

例子

```hs
betterFind (sizeP `equalP` 1024) :: FilePath -> IO [FilePath]
```

## lift

抽象概念的映射: 路径 -> 文件范畴的路径

a 与 b 等到 c 映射到 Info a 与 b 得到 Info c

```hs
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k
```

可以把当前的概念映射到文件范畴的概念

```hs
greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
```

---

映射 and

```hs
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z
```

用原有的 liftP 理解

k 变成了 `g`

```hs
`q` k <--> && g w x y z
```

`w x y z`可以看作是`InfoP`, `=`是 bool

curry 有

```hs
liftP (&&) f :: Bool -> InfoP Bool
simpleAndP f :: InfoP Bool -> InfoP Bool
```

只需要将 Bool 变成 InfoP Bool, 则定义

```hs
constP :: a -> InfoP a
constP k _ _ _ _ = k -- `w x y z`可以看作是`InfoP`, `=`是 bool
```

则有

```hs
simpleAndP f . constP = liftP (&&) f
```

可以看出, `simpleAndP` 是更为基本的

```hs
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z
```

有

```hs
simpleAndP = liftP2 (&&)
liftP q f k = liftP2 q f (constP k) --> liftP2 q f . constP
```

也就是说明`InfoP a -> InfoP b -> InfoP c`变成`InfoP a -> b -> InfoP c` 是比较容易的, 因为会有`b -> InfoP b`

---

```hs
myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False
```

```hs
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)
```

可以定义相应的范畴运算, 类比思考

```hs
(==?) = equalP
(&&?) = andP
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)
```

## Controlling traversal

```hs
data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
```

## Another way of looking at traversal

We can address the weaknesses of our two prior traversal functions by taking a different perspective: what if we think of filesystem traversal as a _fold_ over the directory hierarchy?

> 实现类似 generator 一样的效果

## generic for

对 for 进行了推广

```hs
data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed

    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)
```

不是像之前那样, 生成一个 list, 然后再进行 filter, 而是直接在生成的过程中进行过滤

可以看出`Iterator`是个 monad, 其中`Iterate`是控制流程的 context, 而`seed`是携带着的信息

> 联想到 DNA 与 RNA, 染色体携带着信息, 染色体与`Iterate`, 信息与`seed`

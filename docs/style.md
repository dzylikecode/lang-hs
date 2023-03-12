# style

https://kowainik.github.io/posts/2019-02-06-style-guide

## haddoc

- [haddoc](https://hackage.haskell.org/package/haddock)
- docs: https://haskell-haddock.readthedocs.io/en/latest/
- quick: https://github.com/aisamanra/haddock-cheatsheet/blob/master/haddocks.pdf

  https://github.com/aisamanra/haddock-cheatsheet/blob/master/haddocks.md

## 函数减轻负担

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

`iter`是给内部函数使用的, 内部函数直接调用即可, 重现定义`fold`思考

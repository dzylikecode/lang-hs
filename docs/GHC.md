# GHC

## ghci

- manual: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html

---

load module

```ghci
ghci> :module + Data.List
```

```ghci
ghci> :m + Data.List
```

---

```ghci
ghci> :info (+)
```

---

```ghci
ghci> :set +t
ghci> :unset +t
```

---

`it`的用法

---

`:?`

## ghc

similar to `gcc`

The `-c` option tells **ghc** to only generate object code.

```bash
ghc -c SimpleJSON.hs
```

- `SimpleJSON.hi`

  The former is an _interface file_, in which ghc stores information about the names exported from our module in machine-readable form.

- `SimpleJSON.o`

  The latter is an _object file_, which contains the generated machine code.

---

. The process of generating an executable is called _linking_.

```bash
ghc -o simple Main.hs SimpleJSON.o
```

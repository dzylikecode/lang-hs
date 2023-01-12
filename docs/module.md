# module

## import

```hs
import Data.List
```

all the functions that `Data.List` exports become available in the global namespace

---

```hs
import Data.List (nub, sort)
```

you can selectively import just those functions

---

```hs
import Data.List hiding (nub)
```

You can also choose to import all of the functions of a module except a few select ones.

---

Another way of dealing with name clashes is to do qualified imports.

```hs
import qualified Data.Map
```

`Data.Map.filter` to reference `Data.Map`'s `filter` function

```hs
import qualified Data.Map as M
```

use `M.filter` to reference `Data.Map`'s `filter` function

### GHCI

```bash
ghci> :m + Data.List
:m + Data.List Data.Map Data.Set
```

## export

single file

```hs
-- Func.hs
module Func (exportFunc1, exportFunc2) where

exportFunc = 1

exportFunc = 2

notExportFunc = 3
```

Then import

```hs
import Func
```

`Func.hs` has to be in the same folder that the program that's importing it is in, though.

---

Modules can also be given a hierarchical structures.

## stack

!> We define project to mean a directory that contains a `stack.yaml` file, which specifies how to build a set of packages. We define **package** to be a package with a Cabal file or an Hpack `package.yaml` file.

- [What goes in a Stack package.yaml file?](https://stackoverflow.com/questions/40332040/what-goes-in-a-stack-package-yaml-file)

  This is hpack:

  - [hpack: A modern format for Haskell packages](https://github.com/sol/hpack)

[hid-examples](https://github.com/bravit/hid-examples)

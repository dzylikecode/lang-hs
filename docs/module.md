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

It is followed by the name of the module, which must begin with a capital letter. A source file must have the same base name (the component before the suffix) as the name of the module it contains. This is why our file `SimpleJSON.hs` contains a module named `SimpleJSON`.

!> 没有括号将导出全部, 括号可以看作是 tuple

---

```hs
module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
```

By doing `Shape(..)`, we exported all the value constructors for `Shape`, so that means that whoever imports our module can make shapes by using the `Rectangle` and `Circle` value constructors. It's the same as writing `Shape (Rectangle, Circle)`.

---

We could also opt not to export any value constructors for `Shape` by just writing `Shape` in the export statement. That way, someone importing our module could only make shapes by using the auxilliary functions `baseCircle` and `baseRect`. `Data.Map` uses that approach. You can't create a map by doing `Map.Map [(1,2),(3,4)]` because it doesn't export that value constructor. However, you can make a mapping by using one of the auxilliary functions like `Map.fromList`.

So when we choose not to export them, we just prevent the person importing our module from using those functions, but if some other functions that are exported return a type, we can use them to make values of our custom data types.

Not exporting the value constructors of a data types makes them more abstract in such a way that we hide their implementation. Also, whoever uses our module can't pattern match against the value constructors.

## stack

!> We define project to mean a directory that contains a `stack.yaml` file, which specifies how to build a set of packages. We define **package** to be a package with a Cabal file or an Hpack `package.yaml` file.

- [What goes in a Stack package.yaml file?](https://stackoverflow.com/questions/40332040/what-goes-in-a-stack-package-yaml-file)

  This is hpack:

  - [hpack: A modern format for Haskell packages](https://github.com/sol/hpack)

---

- [hid-examples](https://github.com/bravit/hid-examples)

  良好的 stack 结构

### single file

```txt
.
├── Geometry.hs
└── main.hs
```

- `Geometry.hs`

[Geometry.hs](../example/Learn-You-a-Haskell-for-Great-Good/modules/singleFile/Geometry.hs ":include :type=code hs")

- `main.hs`

[main.hs](../example/Learn-You-a-Haskell-for-Great-Good/modules/singleFile/main.hs ":include :type=code hs")

- `package.yaml`

```yaml
executables:
  singleFile:
    source-dirs: modules/singleFile
    main: main.hs
    other-modules: Geometry
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
internal-libraries:
  Geometry-SingleFile:
    source-dirs: modules/singleFile
    exposed-modules:
      - Geometry
    other-modules: []
```

也可以不需要

```hs
internal-libraries:
  Geometry-SingleFile:
    source-dirs: modules/singleFile
    exposed-modules:
      - Geometry
    other-modules: []
```

同一文件夹下可以找到, 直接

```hs
import Geometry
```

### multiple files

Modules can also be given a hierarchical structures.

```txt
.
├── GeometryM
│   ├── Cube.hs
│   ├── Cuboid.hs
│   └── Sphere.hs
└── main.hs
```

- `Cube.hs`

[Cube.hs](../example/Learn-You-a-Haskell-for-Great-Good/modules/multiFile/GeometryM/Cube.hs ":include :type=code hs")

- `Cuboid.hs`

[Cuboid.hs](../example/Learn-You-a-Haskell-for-Great-Good/modules/multiFile/GeometryM/Cuboid.hs ":include :type=code hs")

- `Sphere.hs`

[Sphere.hs](../example/Learn-You-a-Haskell-for-Great-Good/modules/multiFile/GeometryM/Sphere.hs ":include :type=code hs")

- `main.hs`

[main.hs](../example/Learn-You-a-Haskell-for-Great-Good/modules/multiFile/main.hs ":include :type=code hs")

- `package.yaml`

```yaml
executables:
  multiFile:
    source-dirs: modules/multiFile
    main: main.hs
    other-modules:
      - GeometryM.Cube
      - GeometryM.Cuboid
      - GeometryM.Sphere
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
internal-libraries:
  Geometry-multiFile:
    source-dirs: modules/multiFile
    exposed-modules:
      - GeometryM.Cube
      - GeometryM.Cuboid
      - GeometryM.Sphere
    other-modules: []
```

> 觉得也可以不需要`internal`, internal 是为了给其他文件夹下用的

```hs
import GeometryM.Cube
```

本来就会在当前文件夹下找

也可以注意`source-dirs`, 这个可以是个列表, 增大搜索范围

### internal

```txt
.
├── Html
│   └── Internal.hs
├── Html.hs
├── README.md
└── main.hs
```

```yaml
executables:
  ex07:
    main: main.hs
    source-dirs: ex07
```

[Html.hs](../example/learn-haskell-by-building-a-blog-generator/ex07/Html.hs ":include :type=code hs")

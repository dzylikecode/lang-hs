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

### single file

```txt
.
├── Geometry.hs
└── main.hs
```

- `Geometry.hs`

[Geometry.hs](../example/Learn_You_a_Haskell_for_Great_Good/modules/singleFile/Geometry.hs ":include :type=code hs")

- `main.hs`

[main.hs](../example/Learn_You_a_Haskell_for_Great_Good/modules/singleFile/main.hs ":include :type=code hs")

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

[Cube.hs](../example/Learn_You_a_Haskell_for_Great_Good/modules/multiFile/GeometryM/Cube.hs ":include :type=code hs")

- `Cuboid.hs`

[Cuboid.hs](../example/Learn_You_a_Haskell_for_Great_Good/modules/multiFile/GeometryM/Cuboid.hs ":include :type=code hs")

- `Sphere.hs`

[Sphere.hs](../example/Learn_You_a_Haskell_for_Great_Good/modules/multiFile/GeometryM/Sphere.hs ":include :type=code hs")

- `main.hs`

[main.hs](../example/Learn_You_a_Haskell_for_Great_Good/modules/multiFile/main.hs ":include :type=code hs")

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

## stack

!> We define project to mean a directory that contains a `stack.yaml` file, which specifies how to build a set of packages. We define **package** to be a package with a Cabal file or an Hpack `package.yaml` file.

- [What goes in a Stack package.yaml file?](https://stackoverflow.com/questions/40332040/what-goes-in-a-stack-package-yaml-file)

  This is hpack:

  - [hpack: A modern format for Haskell packages](https://github.com/sol/hpack)

[hid-examples](https://github.com/bravit/hid-examples)

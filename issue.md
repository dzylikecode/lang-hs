# issue

## Haskell 深层文件夹的代码不能被 vscode 正常分析

1. terminal 跳转到相应路径
2. `code .`

## package 路径问题

```yaml
executables:
  singleFile:
    source-dirs: modules/singleFile/main.hs
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

上面会找不到 Geometry 模块, 需要修改成为下面的

```yaml
executables:
  singleFile:
    source-dirs: modules/singleFile # add source dir
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

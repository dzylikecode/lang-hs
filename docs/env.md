# environment

GHC has three main components.

- **ghc** is an optimizing compiler that generates fast native code.
- **ghci** is an interactive interpreter and debugger.
- **runghc** is a program for running Haskell programs as scripts, without needing to compile them first.

## installation

- [Installation](https://www.haskell.org/ghcup/install/#how-to-install)

  !> notice to read system requirements

- [Appendix A. Installing GHC and Haskell libraries](https://book.realworldhaskell.org/read/installing-ghc-and-haskell-libraries.html)

[Installing GHCup vs vanilla GHC for haskell](https://stackoverflow.com/questions/72056777/installing-ghcup-vs-vanilla-ghc-for-haskell) 推荐第一个 GHCup

---

安装依赖:

```bash
sudo apt-get install build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

!> 下载不会很顺畅, 会卡住, 可以再执行一遍

到了 download latest package 会完全卡住, 则完全可以取消, 只用将以下添入环境变量即可

```bash
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
```

然后通过 tui 进行下载即可

```bash
ghcup tui
```

通过`ghcup set --help`可以设置默认的 stack, hls 等

## uninstall

```bash
ghcup nuke
```

## GHCI

ghc interpreter

```bash
ghci
```

---

quit: `Ctrl+D`

---

`:info (+)`

---

`:l fileNameWithOutExt` load the file

`:r fileNameWithOutExt` reload the file

## vscode extension

- Haskell

## stack

- [What is the difference between Cabal and Stack?](https://stackoverflow.com/questions/30913145/what-is-the-difference-between-cabal-and-stack)

  stack is better

```bash
stack update
```

获取 index

docs: https://docs.haskellstack.org/en/stable/

---

```bash
stack new <project_name>
```

有可能不是 stack, 而是`stack-x.x.x`

init

```bash
stack new test new-template -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"
```

---

edit config

path: `$HOME/.stack/config.yaml`

---

使用 qualified

添加

- `-XImportQualifiedPost`选项

---

```hs
import Control.Concurrent.Async
```

- 搜索`Control.Concurrent.Async`
- 查看网页标题: `async-2.2.4: Run IO operations asynchronously and wait for their results`
- 添加信息到 package.yaml:`async`
- `stack install`

!> install 的时候会有文件复制到 `$HOME/.local/bin`中, 自己注意删除

不要使用 install 命令

---

推荐参考

- repos: https://github.com/bravit/hid-examples

## vim

- hlint

- [Haskell Live](http://haskelllive.com/environment.html)

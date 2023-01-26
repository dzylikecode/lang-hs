```txt
ghci> solveRPN "1 2 * 4 +"
Just 6.0
ghci> solveRPN "1 2 * 4 + 5 *"
Just 30.0
ghci> solveRPN "1 2 * 4"
Nothing
ghci> solveRPN "1 8 wharglbllargh"
Nothing
```

想实现类似的, 不需要过度分析, 只需要类似的形式好了

可以参考 command line 的实现方式

- [ ] 实现它

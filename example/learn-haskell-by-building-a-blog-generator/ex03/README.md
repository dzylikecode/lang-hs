将规律的函数进一步抽象

```hs
html_ content = "<html>" <> content <> "</html>"

body_ content = "<body>" <> content <> "</body>"

head_ content = "<head>" <> content <> "</head>"

title_ content = "<title>" <> content <> "</title>"
```

得到统一的抽象, 元素:`el`

```hs
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

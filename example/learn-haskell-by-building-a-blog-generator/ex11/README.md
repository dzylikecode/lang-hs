# Converting Markup to HTML

```hs
convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading 1 txt ->
      Html.h1_ txt
    Markup.Paragraph p ->
      Html.p_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list
    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

`Markup`和`Html`都有`Structure`的概念, 表现了 Haskell 的范畴映射

比如都有 list 的概念, 一个是`-`, 一个是`<ul>`和`<li>`

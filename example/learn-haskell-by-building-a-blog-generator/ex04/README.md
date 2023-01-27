使用类型来区分不同的对象

让编译器帮助检测

You can think of the constructor as a function that takes the argument and returns something of our new type:

```hs
Html :: String -> Html
```

This is useful when we want _encapsulation_. We can define and use existing representation and functions for our underlying type, but not mix them with other, unrelated (to our domain) types. Similar as meters and feet can both be numbers, but we don't want to accidentally add feet to meters without any conversion.

## type

- A complete Html document
- A type for html structures such as headings and paragraphs that can go inside the tag

string 和 elem 都是 string, 为了区分已经是 elem 的 string, 将其包装进入`structure`里面

> 然后围绕着`structure`进行思考, 比如合并

```hs
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)
```

通过

```hs
html_ :: Title -> Structure -> Html
```

将 Structure 转化为合理的 Html, 最后渲染

## Next

Next we'll see how we can make expressions such as Structure "hello" illegal as well using modules and smart constructors.

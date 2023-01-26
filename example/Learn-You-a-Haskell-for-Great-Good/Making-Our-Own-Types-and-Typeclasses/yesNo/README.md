# Yes No

In JavaScript and some other weakly typed languages, you can put almost anything inside an if expression. For example, you can do all of the following: `if (0) alert("YEAH!") else alert("NO!")`, `if ("") alert ("YEAH!") else alert("NO!")`, `if (false) alert("YEAH") else alert("NO!)`, etc. and all of these will throw an alert of `NO!`. If you do `if ("WHAT") alert ("YEAH") else alert("NO!")`, it will alert a `"YEAH!"` because JavaScript considers non-empty strings to be a sort of true-ish value.

## fun

感受到 Haskell 的强类型可以构造弱类型, 更有意思的是可以自由地构造语言

有用最小的系统, 推导出一个生态的意味

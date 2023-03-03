# fold

inspired by https://book.realworldhaskell.org/read/functional-programming.html

## foldable

### fold left

```hs
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero
```

将 step 写成 infix 的形式更好看

```hs
foldl step zero (x:xs) = foldl step (zero `step` x) xs
foldl _    zero []     = zero
```

写成数学展开形式

$$\text{zero} << x_1 << x_2 << \cdots << x_n$$

> `<<` 表示 foldl 的 step, 左结合

### fold right

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = x `step` (foldr step zero xs)
foldr _    zero []     = zero
```

写成数学展开形式

$$x_1 >> x_2 >> \cdots >> x_n >> \text{zero}$$

> `>>` 表示 foldr 的 step, 右结合

!> step 表达了结合方向, `step acc x`从左往右(左结合), `step x acc`从右往左(右结合)

## fold perspective

观察问题

```java
public class Adler32
{
    private static final int base = 65521;

    public static int compute(byte[] data, int offset, int length)
    {
		int a = 1, b = 0;

		for (int i = offset; i < offset + length; i++) {
			a = (a + (data[i] & 0xff)) % base;
			b = (a + b) % base;
		}

		return (b << 16) | a;
    }
}
```

---

使用 Haskell 改写

```hs
adler32_try2 xs = helper (1,0) xs
  where
    helper (a,b) (x:xs) =
	  let a' = (a + (ord x .&. 0xff)) `mod` base
		  b' = (a' + b) `mod` base
	   in helper (a',b') xs
	helper (a,b) _     = (b `shiftL` 16) .|. a
```

---

使用 Haskell 的更为高级的概念来思考, 用 fold 观察

```hs
adler32_try3 xs =
  let (a,b) = foldl step (1,0) xs
   in (b `shiftL` 16) .|. a
  where
	step (a,b) x =
	  let a' = (a + (ord x .&. 0xff)) `mod` base
		  b' = (a' + b) `mod` base
	   in (a',b')
```

可以用数学的形式来思考

$$(a, b) = (1, 0) << x_1 << x_2 << \cdots << x_n$$

## fold application

The class of functions that we can express using `foldr` is called **primitive recursive**.

> 用`foldr`来思考

### filter

```hs
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
```

### map

```hs
myMap f xs = foldr step [] xs
	where step x ys = f x : ys
```

### foldl

```hs
myFoldl f z xs = foldr step id xs z
	where step x g a = g (f a x)
```

better understanding of `step`

```hs
step x g = \a -> g (f a x)
```

证明

$$g_i(a) = a << x_{n-(i-1)} << \cdots << x_n$$

则由归纳:

$$
\begin{aligned}
	g_{i+1}(a) &= x_{n-i} >> g_i(a) \\
	&= g_i(a << x_{n-i}) \\
	&= a << x_{n-i} << x_{n-(i-1)} << \cdots << x_n
\end{aligned}
$$

> `step`相当于`>>`, 而`f`相当于`<<`

---

对于 1+2+3

- 右结合

  `foldr`的直观理解是, 从右往左看, 像一个洋葱由里向外生长, 先里面的, 再外面的

  ```hs
  1 + 2
  1 + (2 + 3)
  1 + (2 + (3 + 0))
  ```

- 左结合

  `foldr`的角度是, 从右往左看, 像一个洋葱由外向里生长, 先外面的, 再里面的

  ```hs
  (? + 3)
  ((? + 2) + 3)
  (((0 + 1) + 2) + 3)
  ```

> 都是从右往左看

---

在设计 `foldr` 的时候, 已经有了 `foldl` 的思想

因为人的阅读习惯是从左往右, 函数的结合方向是从左往右, 所以构造 `foldl` 非常自然

```hs
foldl step zero (x:xs) = foldl step (zero `step` x) xs
foldl _    zero []     = zero
```

由$\text{acc}'=\text{acc} << x$, 从左往右迭代下去

```hs
foldr step zero (x:xs) = x `step` (foldr step zero xs)
foldr _    zero []     = zero
```

利用未知量$?$去假设$\text{acc}=x >> ? = x >> (x' >> ?)$, 从左往右不断递推下去

> 未知量可以用函数的参数来表示

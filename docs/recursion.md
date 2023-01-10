# recursion

Recursion is actually a way of defining functions in which the function is applied inside its own definition.

---

Recursion is important to Haskell because unlike imperative languages, you do computations in Haskell by declaring what something is instead of declaring how you get it. That's why there are no while loops or for loops in Haskell and instead we many times have to use recursion to declare what something is.

## edge condition

Having an element or two in a recursion definition defined non-recursively (like _F(0)_ and _F(1)_ here) is also called the _edge condition_ and is important if you want your recursive function to terminate.

## infinite recursion

Because Haskell supports infinite lists, our recursion doesn't really have to have an edge condition.

```hs
repeat' :: a -> [a]
repeat' x = x:repeat' x
```

```hs
take 3 (repeat' 5)
-- [5,5,5]
```

由于 Haskell 的 lazy 性质, take 会终结`repeat'`的运算

## thinking recursively

We did quite a bit of recursion so far and as you've probably noticed, there's a pattern here. Usually you define an edge case and then you define a function that does something between some element and the function applied to the rest.

So when trying to think of a recursive way to solve a problem, try to think of when a recursive solution doesn't apply and see if you can use that as an edge case, think about identities and think about whether you'll break apart the parameters of the function (for instance, lists are usually broken into a head and a tail via pattern matching) and on which part you'll use the recursive call.

# test

Property-based testing encourages a high level approach to testing in the form of abstract invariants functions should satisfy universally, with the actual test data generated for the programmer by the testing library.

One useful invariant to start with, and one that comes up in a lot of purely functional code, is idempotency — applying a function twice has the same result as applying it only once.

```hs
prop_minimum xs         = head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
```

真的是有意思

## abstract

- The behaviors documenting the use of your library should be written with [doctest](https://github.com/sol/doctest-haskell#readme)
- The behaviors documenting functionality rather than usage of your library should be written with [hspec](http://hspec.github.com/)
- Cabal is used to automate testing with the frameworks
- For pure code, QuickCheck property test cases (with doctest and/or hspec) should be written as much as possible

## References

1. [unit-test-example](https://github.com/kazu-yamamoto/unit-test-example)

   [Up-front Unit Testing in Haskell](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md)

2. [sydtest](https://github.com/NorfairKing/sydtest)

3. [hspec](https://github.com/hspec/hspec)

   doc: https://hspec.github.io/

4. [Chapter 11. Testing and quality assurance](https://book.realworldhaskell.org/read/testing-and-quality-assurance.html#id628218)

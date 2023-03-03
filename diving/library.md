# library

As we write our first substantial body of code, it's a huge help to pause every few minutes and try to compile what we've produced so far. Because Haskell is so strongly typed, if our code compiles cleanly, we're assuring ourselves that we're not wandering too far off into the programming weeds.

One useful technique for quickly developing the skeleton of a program is to write placeholder, or _stub_ versions of types and functions. For instance, we mentioned above that our `string`, `text` and `double` functions would be provided by our `Prettify` module. If we don't provide definitions for those functions or the Doc type, our attempts to "compile early, compile often" with our JSON renderer will fail, as the compiler won't know anything about those functions. To avoid this problem, we write stub code that doesn't do anything.

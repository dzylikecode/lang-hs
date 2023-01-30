# Internal modules

For that we can expose internal modules to provide some flexibility for advanced users. Internal modules are not a language concept but rather a (fairly common) design pattern (or idiom) in Haskell.

Internal modules are simply modules named `<something>.Internal`, which export all of the functionality and implementation details in that module.

Instead of writing the implementation in (for example) the `Html` module, we write it in the `Html.Internal` module, which will export everything. Then we will import that module in the `Html` module, and write an explicit export list to only export the API we'd like to export (as before).

`Internal` modules are considered unstable and risky to use by convention. If you end up using one yourself when using an external Haskell library, make sure to open a ticket in the library's repository after the storm has passed!

main = putStrLn (wrapHtml "Hello, world!")

wrapHtml content = "<html><body>" <> content <> "</body></html>"
main = putStrLn myhtml

myhtml :: String
myhtml =
  makeHtml
    "Hello title"
    (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")

makeHtml title content = html_ (head_ (title_ title) <> body_ content)

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ = el "html"

body_ = el "body"

head_ = el "head"

title_ = el "title"

p_ = el "p"

h1_ = el "h1"
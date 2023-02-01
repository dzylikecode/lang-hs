import Markup

sampleFileIn = "example/learn-haskell-by-building-a-blog-generator/assets/sample_in.txt"

sampleFileOut = "example/learn-haskell-by-building-a-blog-generator/assets/sample_out.txt"

main = do
  txt <- readFile sampleFileIn
  let parsed = Markup.parse txt
  print parsed

--   cmp <- readFile sampleFileOut
--   print $ parsed == read cmp

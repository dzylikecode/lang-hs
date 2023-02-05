decorate num = "wk" <> prefix num
  where
    prefix n =
      if n < 10
        then "0" <> show n
        else show n

link (name, src) = "- [" <> name <> "](" <> src <> ")"

title num = "week " <> show num

path num = "/Princeton/docs/" <> decorate num <> ".md"

generate = map $ link . (\n -> (title n, path n))

main = interact $ unlines . generate . (\n -> [0 .. n]) . toInt
  where
    toInt = read :: String -> Integer
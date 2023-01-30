parse :: String -> Document
parse = parseLines [] . lines -- (1)

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
   in case txts of -- (4)
        [] -> [paragraph]
        currentLine : rest ->
          if currentLine |> isEndofParagraph
            then paragraph : parseLines [] rest -- (5)
            else parseLines (currentLine : currentParagraph) rest -- (6)
  where
    isEndofParagraph = (== "") . trim

trim :: String -> String
trim = unwords . words

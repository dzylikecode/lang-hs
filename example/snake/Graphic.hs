draw :: Char -> Position -> IO ()
draw char (row, col) = do
  setCursorPosition row col
  putChar char
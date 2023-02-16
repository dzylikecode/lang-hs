drawBorder :: World -> IO ()
drawBorder w = do
  let (r, c) = limits w
  mapM_ (draw '*') [(0, x) | x <- [0 .. c + 1]]
  mapM_ (draw '*') [(r + 1, x) | x <- [0 .. c + 1]]
  mapM_ (draw '*') [(x, 0) | x <- [0 .. r + 1]]
  mapM_ (draw '*') [(x, c + 1) | x <- [0 .. r + 1]]

renderWorld :: Char -> Char -> World -> IO ()
renderWorld s f w = do
  draw f (food w)
  mapM_ (draw s) (reverse $ snake w)
  cursorBackward 1

drawWorld = renderWorld '@' '#'

clearWorld = renderWorld ' ' ' '
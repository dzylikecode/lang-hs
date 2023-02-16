module Config where

initialWorld =
  World
    { snake = [(5, x) | x <- [10 .. 13]],
      food = (5, 5),
      direction = West,
      rand = R.mkStdGen 0,
      limits = (20, 20)
    }

initScreen = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  clearScreen

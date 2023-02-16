type Position = (Int, Int)

type Snake = [Position]

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

opposite :: Direction -> Direction
opposite d = case d of
  North -> South
  South -> North
  East -> West
  West -> East

move :: Direction -> Position -> Position
move d (r, c) = case d of
  North -> (r - 1, c)
  South -> (r + 1, c)
  East -> (r, c + 1)
  West -> (r, c - 1)

slither :: Snake -> Direction -> Snake
slither d s = (move d $ head s) : (init s)

eat :: Snake -> Direction -> Snake
eat d s = (move d $ head s) : s
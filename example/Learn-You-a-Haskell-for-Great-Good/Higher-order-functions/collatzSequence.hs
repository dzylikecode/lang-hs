dzChain :: Integral a => a -> [a]
dzChain 1 = [1]
dzChain n
  | even n = n : dzChain (n `div` 2)
  | odd n = n : dzChain (n * 3 + 1)
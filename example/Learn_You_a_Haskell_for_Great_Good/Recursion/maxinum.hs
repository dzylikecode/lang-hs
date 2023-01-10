dzMaximum :: (Ord a) => [a] -> a
dzMaximum [] = error "maximum of empty list"
dzMaximum [x] = x
dzMaximum (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = dzMaximum xs

-- elegant

dzMaximum' :: Ord a => [a] -> a
dzMaximum' [] = error "maximum of empty list"
dzMaximum' [x] = x
dzMaximum' (x : xs) = max x (dzMaximum' xs)
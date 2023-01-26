dzSum :: (Num a) => [a] -> a
dzSum xs = foldl (\acc x -> acc + x) 0 xs

-- equals
dzSum' :: (Num a) => [a] -> a
dzSum' = foldl (+) 0

-----------------------------------------------------------

-- accumulate 的逻辑
-- 如果为True, 则为True
-- 如果为False, 不影响, 继续判断
dzElem :: (Eq a) => a -> [a] -> Bool
dzElem y ys = foldl accumulate False ys
  where
    accumulate = \acc x ->
      if x == y
        then True
        else acc

dzElem' :: (Eq a) => a -> [a] -> Bool
dzElem' y = foldl accumulate False
  where
    accumulate acc x = (x == y) || acc

-----------------------------------------------------------

dzMap :: (a -> b) -> [a] -> [b]
dzMap f xs = foldr (\x acc -> f x : acc) [] xs

dzMap' :: (a -> b) -> [a] -> [b]
dzMap' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-----------------------------------------------------------

dzFoldl1 :: (a -> a -> a) -> [a] -> a
dzFoldl1 f (x : xs) = foldl f x xs

-----------------------------------------------------------

dzMaximum :: (Ord a) => [a] -> a
dzMaximum = foldr1 (\x acc -> if x > acc then x else acc)

-----------------------------------------------------------

dzReverse :: [a] -> [a]
dzReverse = foldl (\acc x -> x : acc) []

dzReverse' :: [a] -> [a]
dzReverse' = foldl (flip (:)) []

-----------------------------------------------------------

dzProduct :: (Num a) => [a] -> a
dzProduct = foldr1 (*)

-----------------------------------------------------------

dzFilter :: (a -> Bool) -> [a] -> [a]
dzFilter p = foldr (\x acc -> if p x then x : acc else acc) []

-----------------------------------------------------------

dzHead :: [a] -> a
dzHead = foldr1 (\x _ -> x)

-- \x _ -> x = const

-- 初始的第一个就是acc, 一直保留下去
dzHead' :: [a] -> a
dzHead' = foldl1 (\acc _ -> acc)

-----------------------------------------------------------

dzLast :: [a] -> a
dzLast = foldl1 (\_ x -> x)

-----------------------------------------------------------

-- 有一点点meta program, decorate的感觉
dzScanl :: (a -> a -> a) -> a -> [a] -> [a]
dzScanl f zero xs = foldl step [zero] xs
  where
    step acc x =
      let prevAcc = last acc
       in acc ++ [f prevAcc x]

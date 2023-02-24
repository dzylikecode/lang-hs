-- 1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard `length` function.

length' [] = 0
length' (x:xs) = 1 + length' xs

-- 2. Add a type signature for your function to your source file. To test it, load the source file into ghci again.

length'' :: [a] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

-- 3. Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- 4. Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list `[1,2,3]`, your function should return `[1,2,3,3,2,1]`.

palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ reverse xs

-- 5. Write a function that determines whether its input list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == reverse xs

-- 6. Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the `sortBy` function from the `Data.List` module.)

import Data.List

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\xs ys -> compare (length xs) (length ys))

-- 7. Define a function that joins a list of lists together using a separator value.

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ [xs] = xs
intersperse' sep (xs:xss) = xs ++ [sep] ++ intersperse' sep xss

-- 8. Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an `Empty`. For example, the tree `Empty` has height zero; `Node "x" Empty Empty` has height one; `Node "x" Empty (Node "y" Empty Empty)` has height two; and so on.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- 9. Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a `Direction` data type that lets you represent these possibilities.

data Direction = Left | Right | Straight deriving (Show)

-- 10. Write a function that calculates the turn made by three 2D points and returns a `Direction`.

turn :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Direction
turn (x1, y1) (x2, y2) (x3, y3)
  | crossProduct > 0 = Left
  | crossProduct < 0 = Right
  | otherwise = Straight
  where crossProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- 11. Define a function that takes a list of 2D points and computes the direction of each successive triple. Given a list of points `[a,b,c,d,e]`, it should begin by computing the turn made by `[a,b,c]`, then the turn made by `[b,c,d]`, then `[c,d,e]`. Your function should return a list of `Direction`.

directions :: (Num a, Ord a) => [(a, a)] -> [Direction]
directions [] = []
directions [_] = []
directions [_, _] = []
directions (x:y:z:xs) = turn x y z : directions (y:z:xs)

-- 12. Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull. is, and how the Graham scan algorithm should work, on Wikipedia.
-- http://en.wikipedia.org/wiki/Graham_scan
-- http://en.wikipedia.org/wiki/Convex_hull
-- http://en.wikipedia.org/wiki/Convex_hull_algorithms


-- answer: https://wiki.haskell.org/Graham_Scan_Implementation
-- proof: https://www.inf.ed.ac.uk/teaching/courses/ads/Lectures/lec16.pdf




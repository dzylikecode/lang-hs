-- 1. Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail. As a hint, you might want to consider using the following types.

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs

-- 2. Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False.

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let (first, rest) = break p xs
                 in first : case rest of
                              [] -> []
                              xs' -> splitWith p (tail xs')

-- 3. Using the command framework from this chapter, write a program that prints the first word of each line of its input.

import System.Environment
import System.IO
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      contents <- readFile input
      mapM_ putStrLn $ map (head . words) $ lines contents
    _ -> do
      hPutStrLn stderr "Usage: exercise3 <input>"
      exitWith $ ExitFailure 1

-- 4. Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

import System.Environment
import System.IO
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      contents <- readFile input
      mapM_ putStrLn $ transpose $ lines contents
    _ -> do
      hPutStrLn stderr "Usage: exercise4 <input>"
      exitWith $ ExitFailure 1


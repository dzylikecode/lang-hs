data List a
  = Cons a (List a)
  | Nil
  deriving (Show)

fromList' :: [a] -> List a
fromList' (x : xs) = Cons x (fromList' xs)
fromList' [] = Nil

data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []

-- exercise 2:
-- Define a tree type that has only one constructor, like our Java example. Instead of the `Empty` constructor, use the Maybe type to refer to a node's children.
data Tree' a
  = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
  deriving (Show)
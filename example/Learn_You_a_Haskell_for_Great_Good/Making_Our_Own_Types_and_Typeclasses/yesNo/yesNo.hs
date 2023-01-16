class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  -- id:
  -- It's just a standard library function that
  -- takes a parameter and returns the same thing,
  -- which is what we would be writing here anyway.
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

data TrafficLight = Red | Yellow | Green

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
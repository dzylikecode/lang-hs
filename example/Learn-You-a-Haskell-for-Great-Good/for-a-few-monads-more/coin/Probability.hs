module Probability
  ( Prob (Prob, getProb),
    flatten,
  )
where

import Control.Monad
import Data.Ratio

newtype Prob a = Prob {getProb :: [(a, Rational)]}
  deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where
    multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  (<*>) = ap

instance Monad Prob where
  return = pure
  m >>= f = flatten (fmap f m)

instance MonadFail Prob where
  fail _ = Prob []

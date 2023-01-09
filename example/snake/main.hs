module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever)
import Data.Monoid ((<>))
import Pipes
import Pipes.Concurrent
import Pipes.Prelude qualified as P
import System.Console.ANSI
import System.IO
import System.Random qualified as R

main = putStrLn "Hello, World!"
module Main where

import           Control.Monad
import qualified Example.HasServerExample              as G

main :: IO ()
main = void G.runExample

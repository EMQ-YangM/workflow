module Main where

import           Control.Monad
import qualified HasServerExample              as G
import qualified MyLib                          ( someFunc )

main :: IO ()
main = void G.runExample

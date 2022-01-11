module Main where

import           Control.Monad
import qualified GenServerExample              as G
import qualified MyLib                          ( someFunc )

main :: IO ()
main = void G.runExample

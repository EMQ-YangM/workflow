module Main where

import           Control.Monad                  ( void )
import qualified Example.HasServerExample      as G

main :: IO ()
main = void G.runExample

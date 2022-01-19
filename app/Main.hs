module Main where

import           Control.Monad                  ( void )
import qualified Example.Example1.HasServerExample      as G

main :: IO ()
main = void G.runExample

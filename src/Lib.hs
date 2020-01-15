module Lib
    ( Orientation(..)
    , rover
    , demo
    ) where

data Orientation = North | South | West | East deriving (Show, Eq)

rover = ((0,0), North)

demo :: IO ()
demo = putStrLn "Not implemented yet..."

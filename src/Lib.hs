module Lib
    ( Orientation(..)
    , initialRover
    , execute
    , demo
    ) where

data Orientation = North | South | West | East deriving (Show, Eq)

initialRover = ((0,0), North)

-- TODO remove replication
execute 'F' ((x,y), North) = ((x,y+1), North)
execute 'F' ((x,y), South) = ((x,y-1), South)
execute 'F' ((x,y), West) = ((x-1,y), West)
execute 'F' ((x,y), East) = ((x+1,y), East)
execute 'B' ((x,y), North) = ((x,y-1), North)
execute 'B' ((x,y), South) = ((x,y+1), South)
execute 'B' ((x,y), West) = ((x+1,y), West)
execute 'B' ((x,y), East) = ((x-1,y), East)

execute 'L' (p, o) = (p, left o)
execute 'R' (p, o) = (p, right o)

left North = East
left East = South
left South = West
left West = North

right North = West
right West = South
right South = East
right East = North

demo :: IO ()
demo = putStrLn "Not implemented yet..."

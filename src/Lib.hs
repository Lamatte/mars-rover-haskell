module Lib
    ( Orientation(..)
    , Rover(..)
    , execute
    , pilot
    , Planet(..)
    , demo
    , showPlanet
    ) where

data Orientation = North | South | West | East deriving (Show, Eq)
data Rover = Rover ((Int, Int), Orientation) deriving (Show, Eq)
data Planet = Planet {size :: Int} deriving (Eq)

showPlanet planet rover = concat(map (showRow planet rover)  (reverse [0..(planetSize planet)-1]))

showRow planet rover y = concat (map (showCell planet rover y) [0..(planetSize planet)-1]) ++ "\n"

showCell planet (Rover ((xr, yr), o)) y x 
    | (x,y) == (xr, yr) = showOrientation o
    | otherwise = "."

showOrientation North = "^"
showOrientation South = "v"
showOrientation East = ">"
showOrientation West = "<"

planetSize (Planet size) = size

pilot commands rover = pilotLog commands rover ""

pilotLog (command:commands) rover log = pilotLog commands (execute command rover) (logCommand command log)
pilotLog "" rover log = log++" -> "++(show rover)

logCommand command "" = [command]
logCommand command log = concat [log, ", ", [command]]

-- TODO remove replication
execute 'F' (Rover ((x,y), North)) = Rover ((x,y+1), North)
execute 'F' (Rover ((x,y), South)) = Rover ((x,y-1), South)
execute 'F' (Rover ((x,y), West)) = Rover ((x-1,y), West)
execute 'F' (Rover ((x,y), East)) = Rover ((x+1,y), East)
execute 'B' (Rover ((x,y), North)) = Rover ((x,y-1), North)
execute 'B' (Rover ((x,y), South)) = Rover ((x,y+1), South)
execute 'B' (Rover ((x,y), West)) = Rover ((x+1,y), West)
execute 'B' (Rover ((x,y), East)) = Rover ((x-1,y), East)

execute 'L' (Rover (p, o)) = Rover (p, left o)
execute 'R' (Rover (p, o)) = Rover (p, right o)

left North = West
left West = South
left South = East
left East = North

right North = East
right East = South
right South = West
right West = North

demo :: IO ()
demo = putStrLn "Not implemented yet..."

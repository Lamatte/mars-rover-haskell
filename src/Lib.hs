module Lib
    ( Orientation(..)
    , initialRover
    , execute
    , pilot
    , demo
    ) where

data Orientation = North | South | West | East deriving (Show, Eq)

initialRover = ((0,0), North)

pilot commands rover = pilotLog commands rover ""

pilotLog (command:commands) rover log = pilotLog commands (execute command rover) (logCommand command log)
pilotLog "" rover log = log++" -> "++(show rover)

logCommand command "" = [command]
logCommand command log = concat [log, ", ", [command]]

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

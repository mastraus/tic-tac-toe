--removed import Data.Char and data Board = [[Cell]]

cleaned up funct
placePiece :: Int -> a -> [a] -> [a]
placePiece input xo board = if (input == 8) 
    then take input board ++ [xo] ++ drop (input+1) board 
    else take input board ++ [xo] ++ drop (input+1) board

--removed import Data.Foldable
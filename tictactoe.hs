import System.IO
import Data.List

--Setting up data, instances--

data Player = X | O
    deriving (Eq)
data Cell = Occupied Player | Open Int
    deriving(Eq)

instance Show Player where
    show X = "X"
    show O = "O"

instance Show Cell where
    show (Occupied a) = show a
    show (Open a) = show a

--Generating grid--

renderRows :: [Cell] -> String
renderRows (a:b:c:xs) = (show a) ++ " | " ++ (show b) ++ " | " ++ (show c)

columnDivider :: String
columnDivider = "----------"

renderBoard :: [Cell] -> IO ()
renderBoard board = do
    putStrLn $ renderRows top
    putStrLn columnDivider
    putStrLn $ renderRows middle
    putStrLn columnDivider
    putStrLn $ renderRows bottom
    where
        top = take 3 board
        middle = drop 3 . take 6 $ board
        bottom = drop 6 board

--Functions to check location's validity--

cellIsOpen :: [Cell] -> Int -> Bool
cellIsOpen board input = if (input == 9) && board !! 8 == (Open input) || board !! (input-1) == (Open input) 
    then True 
    else False

placePiece :: Int -> a -> [a] -> [a]
placePiece input xo board = take input board ++ [xo] ++ drop (input+1) board

--Compares all vertical, horizonal, and diagonal options to see if any generate a 'true' response, indicating a winner--

checkForWinner :: Player -> [Cell] -> Bool
checkForWinner xo board = vertical xo board || horizonal xo board || diagonal xo board

vertical :: Player -> [Cell] -> Bool
vertical xo board = 
    if 
        board!!0 == (Occupied xo) && board!!3 == (Occupied xo) && board!!6 == (Occupied xo) || board!!1 == (Occupied xo) && board!!4 == (Occupied xo) && board!!7 == (Occupied xo) || board!!2 == (Occupied xo) && board!!5 == (Occupied xo) && board!!8 == (Occupied xo) 
    then True 
    else False

horizonal :: Player -> [Cell] -> Bool
horizonal xo board = 
    if 
        board!!0 == (Occupied xo) && board!!1 == (Occupied xo) && board!!2 == (Occupied xo) || board!!3 == (Occupied xo) && board!!4 == (Occupied xo) && board!!5 == (Occupied xo) || board!!6 == (Occupied xo) && board!!7 == (Occupied xo) && board!!8 == (Occupied xo) 
    then True 
    else False

diagonal :: Player -> [Cell] -> Bool
diagonal xo board = 
    if 
        board!!0 == (Occupied xo) && board!!4 == (Occupied xo) && board!!8 == (Occupied xo) || board!!2 == (Occupied xo) && board!!4 == (Occupied xo) && board!!6 == (Occupied xo) 
    then True 
    else False

--Gameplay: switching players and game flow--

changePlayers :: Player -> Player
changePlayers X = O
changePlayers O = X

runGame :: Player -> [Cell] -> Int -> IO ()
runGame player board guessCount = do
    if guessCount == 9
        then putStrLn "It's a tie!"
    else do
        putStrLn $ " "
        renderBoard board
        putStrLn $ (show player) ++ " 's turn, type the space you'd like to select:"
        input <- readLn::IO Int
        if cellIsOpen board input then do
            let newBoard = placePiece (input-1) (Occupied player) board
            putStrLn $ " "
            renderBoard newBoard
            putStrLn $ " "
            if checkForWinner player newBoard then do
                putStrLn $ (show player) ++ " is the Winner! Type 'main' to play again."
            else do
                runGame (changePlayers player) newBoard (guessCount + 1)
        else do
            putStrLn $ " "
            putStrLn $ "Error: not valid"
            putStrLn $ " "
            runGame player board (guessCount + 0)

main :: IO ()
main = do
    putStrLn  "\n*~* Welcome to A 'Qwick' Game of Tic Tac Toe! *~*\n"
    runGame X board 0
        where board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]
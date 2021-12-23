import System.IO
import Data.List

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

--xs is a built-in mechanic for applying outputs to lists. renderRows, when invoked, will take the 3 corresponding values from the list 'board' (seen in the renderBoard function) and apply each sequentially (a/index 1 of the 3, b/index 2 of the 3,...)
renderRows :: [Cell] -> String
renderRows (a:b:c:xs) = (show a) ++ " | " ++ (show b) ++ " | " ++ (show c)

columnDivider :: String
columnDivider = "----------"

--prints on the CLI the outcome of invoking renderRows in groups of 3, then applying the divider beneath each
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

--since the board's index naturally runs 1 less than the smallest number (there is no 0 on the board), input-1 gets the index from the board and compares it to if that number equals an Open spot. placePiece can then be invoked, taking the index again, adding the player's symbol of x or o, and dropping the index after (which would be the number it was replacing)
cellIsOpen :: [Cell] -> Int -> Bool
cellIsOpen board input = if board !! (input-1) == (Open input) 
    then True 
    else False

placePiece :: Int -> a -> [a] -> [a]
placePiece input xo board = take input board ++ [xo] ++ drop (input+1) board

--returns 'true' if vertical, horizontal, or diagonal return 'true' findings; if the groups of 3 indexed numbers all equal the same Occupied symbol of either x/o, it will return true. These functions were broken up to increase readability.
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

changePlayers :: Player -> Player
changePlayers X = O
changePlayers O = X

runGame :: Player -> [Cell] -> Int -> IO ()
runGame player board guessCount = do
--guessCount increases +1 each time the game is ran inside the block (therefore increasing with each guess) because there are a maximum of 9 guesses. A total of 9 successful guesses automatically indicates a tied game when the board is filled and no winner is declared.
    if guessCount == 9
        then putStrLn "It's a tie!"
    else do
        putStrLn $ " "
        renderBoard board
        putStrLn $ (show player) ++ " 's turn, type the space you'd like to select:"
        input <- readLn::IO Int
        if cellIsOpen board input && input < 10 then do
            let newBoard = placePiece (input-1) (Occupied player) board
            if checkForWinner player newBoard then do
                putStrLn $ (show player) ++ " is the Winner! Type 'main' to play again."
            else do
                runGame (changePlayers player) newBoard (guessCount + 1)
--error code block runs if selected cell is not open or the input is greater than 10.
        else do
            putStrLn $ " "
            putStrLn $ "ERROR: Entry not valid. Try again."
            putStrLn $ " "
            runGame player board (guessCount + 0)

main :: IO ()
main = do
    putStrLn  "\n*~* Welcome to A 'Qwick' Game of Tic Tac Toe! *~*\n"
    runGame X board 0
        where board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]
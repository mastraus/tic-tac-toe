import System.IO
import Data.List
import Data.Char

data Player = X | O
data Cell = Occupied Player | Open Int

instance Show Player where
    show X = "X"
    show O = "O"

instance Show Cell where
    show (Occupied X) = "X"
    show (Occupied O) = "O"
    show (Open a) = show a

instance Eq Cell where
    Occupied X == Occupied X = True
    Occupied O == Occupied O = True
    Open a == Open b = True
    _ == _ = False

type Board = [[Cell]]

changePlayers :: Player -> Player
changePlayers X = O
changePlayers O = X

board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]

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

cellIsOpen :: [Cell] -> Int -> Bool
cellIsOpen board input = if board !! input == Open input then True else False

placePiece :: Int -> a -> [a] -> [a]
placePiece input xo board = take input board ++ [xo] ++ drop (input+1) board

--placePiece :: Int -> a -> [a] -> [a] ->
--placePiece input xo board = take input board ++ [xo] ++ drop (input+1) board

runGame :: Player -> [Cell] -> IO ()
runGame player board = do
    --prompts the user to do stuff when game starts--
    putStrLn $ " "
    print board
    putStrLn $ (show player) ++ " 's turn, type the space you'd like to select:"
-- get line with the arrow takes the input from the command line and stores it as userGuess--
    input <- readLn::IO Int
    if input `elem` [1..9] && cellIsOpen board input then do
        let newBoard = placePiece (input-1) (Occupied player) board
        putStrLn $ " "
        print newBoard
        putStrLn $ " "
        runGame (changePlayers player) newBoard
 --       if (isWinner newBoard (Occupied player)) then do
  --          renderBoard newBoard
  --          putStrLn $ "Winning one rendered"
  --      else do
   --         putStrLn $ " "
   --         putStrLn "Yo"
    else do
        putStrLn $ " "
        putStrLn $ "Error: not valid"
        putStrLn $ " "
        runGame player board
--    checkPlayerWin player newBoard

--    if userInput `elem` ['1' .. '9'] && openCell board (read [userInput])
 --       then let newBoard = placePlayer userInput Player board
--    if check for ending to game
 --       then
 --           else runGame again with the next player

main :: IO ()
main = do
    putStrLn  "\n*~* Welcome to A 'Qwick' Game of Tic Tac Toe! *~*\n"
    runGame X board
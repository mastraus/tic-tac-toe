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

--removeNth :: Int -> [a] -> ([a], [a])
--removeNth index list =(left,right)
  --  where
    --    (left, ys) = splitAt (index - 1) list
      --  right = drop 1 ys

-- Given a board, piece, and index to place it in, place piece
-- at the position N (index being N -1)
--placePiece :: [a] -> a -> Int -> [a]
--placePiece board player index = xs ++ [player] ++ ys
  --  where (xs, ys) = removeNth index board

placePiece :: Int -> a -> [a] -> [a]
placePiece i x xs = take i xs ++ [x] ++ drop (i+1) xs

--openCell :: [Cell] -> Bool
--openCell (Open _) = True
--openCell _ = False

--placePlayer :: String -> Player -> [Cell]
--placePlayer location playerMark board

runGame :: Player -> [Cell] -> IO ()
runGame player board = do
    --prompts the user to do stuff when game starts--
    renderBoard board
    putStrLn $ (show player) ++ " 's turn, type the space you'd like to select:"
-- get line with the arrow takes the input from the command line and stores it as userGuess--
    input <- getChar
    if input `elem` ['1' .. '9']
        then (read [input])
    let inp = ord input
 --   let input = ord userInput
    let newBoard = placePiece (inp-1) (Occupied player) board
    putStrLn $ " "
    print newBoard
    putStrLn $ "Nice"
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
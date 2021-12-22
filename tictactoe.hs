data Player = X | O
    deriving (Show)
data Cell = Occupied Player | Open Int

instance Show Cell where
    show (Open a) = show a
    show (Occupied b) = show b

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

runGame :: Player -> [Cell] -> IO ()
runGame player board = do
    --prompts the user to do stuff when game starts--
    renderBoard board
    putStrLn $ (show player) ++ " 's turn, type the space you'd like to select:"
-- get line with the arrow takes the input from the command line and stores it as userGuess--
    userInput <- getLine
    putStrLn userInput
--    if check for ending to game
 --       then
 --           else runGame again with the next player

main :: IO ()
main = do
    putStrLn "\n*~* Welcome to A 'Qwick' Game of Tic Tac Toe! *~*\n"
    runGame X board
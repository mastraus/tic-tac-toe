{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO
import Data.List

--The 'data' and 'instance' declarations were very new for me, so I implemented them from patterns I saw in other examples. After playing around with different iterations of Show and Eq (I tried doing deriving (Eq,Show) for a while), I decided on this.
data Player = X | O
    deriving (Eq)

data Cell = Occupied Player | Open Int
    deriving(Eq)

instance Show Player where
    show X = "X"
    show O = "O"

instance Show Cell where
    show (Occupied a) = show a
    show (Open a)     = show a

--xs is a built-in mechanic for applying outputs to lists. renderRows, when invoked, will take the 3 corresponding values from the list 'board' (seen in the renderBoard function) and apply each sequentially (a/index 1 of the 3, b/index 2 of the 3,...)
renderRows :: [Cell] -> String
renderRows (a:b:c:xs) = show a ++ " | " ++ show b ++ " | " ++ show c

--Prints on the CLI the outcome of invoking renderRows in groups of 3, then applying the divider beneath each. Initially I had the divider in it's own function (columnDivider) but decided to print to the screen so renderBoard was not invoking as many functions. It would be interesting to see if this has any affect on performance. I also wanted to find a more 'elegant' method to create a grid, but a lot of the syntax and functions were very difficult to understand, and I prioritized the code being easy-to-read (mostly for my benefit when I come back to review it.)
renderBoard :: [Cell] -> IO ()
renderBoard board = do
    putStrLn $ renderRows top
    putStrLn "----------"
    putStrLn $ renderRows middle
    putStrLn "----------"
    putStrLn $ renderRows bottom
    where
        top    = take 3 board
        middle = drop 3 . take 6 $ board
        bottom = drop 6 board

--Since the board's index naturally runs 1 less than the smallest number (there is no 0 on the board), input-1 gets the index from the board and compares it to if that number equals an Open spot. placePiece can then be invoked, taking the index again, adding the player's symbol of x or o, and dropping the index after (which would be the number it was replacing.) I had the most trouble playing with inputs of 1 and 9 before realizing I would have to manually adjust the input to attribute for the index (0 wouldn't be an Open space on the board and 9 wasn't an index.)
cellIsOpen :: [Cell] -> Int -> Bool
cellIsOpen board input = board !! (input-1) == Open input

placePiece :: Int -> a -> [a] -> [a]
placePiece input xo board = take input board ++ [xo] ++ drop (input+1) board

--Returns 'true' if vertical, horizontal, or diagonal return 'true' findings; if the groups of 3 indexed numbers all equal the same Occupied symbol of either x/o, it will return true. These functions were broken up to increase readability. While I tried a lot of methods for checking the winner, I realized that iterating through my array was more challenging due to the fact it had the Cell type embedded and didn't respond to a lot of the methods I researched, many of which were best for lists with just arrays or specific rows and columns. One method I played with for a while was foldr and foldl as well as attempting to loop through the array and compare each indexed value to each other. I was not having success so decided to write a much more "brute" method of explicitally checking. On a future iteration of this project, this is the feature I'd be most interested in refining.
checkForWinner :: Player -> [Cell] -> Bool
checkForWinner xo board = vertical xo board || horizonal xo board || diagonal xo board

vertical :: Player -> [Cell] -> Bool
vertical xo board =
    head board == Occupied xo && board!!3 == Occupied xo && board!!6 == Occupied xo
    || board!!1 == Occupied xo && board!!4 == Occupied xo && board!!7 == Occupied xo 
    || board!!2 == Occupied xo && board!!5 == Occupied xo && board!!8 == Occupied xo

horizonal :: Player -> [Cell] -> Bool
horizonal xo board =
    head board == Occupied xo && board!!1 == Occupied xo && board!!2 == Occupied xo 
    || board!!3 == Occupied xo && board!!4 == Occupied xo && board!!5 == Occupied xo 
    || board!!6 == Occupied xo && board!!7 == Occupied xo && board!!8 == Occupied xo

diagonal :: Player -> [Cell] -> Bool
diagonal xo board =
    head board == Occupied xo && board!!4 == Occupied xo && board!!8 == Occupied xo 
    || board!!2 == Occupied xo && board!!4 == Occupied xo && board!!6 == Occupied xo

changePlayers :: Player -> Player
changePlayers X = O
changePlayers O = X

runGame :: Player -> [Cell] -> Int -> IO ()
runGame player board guessCount =
    if guessCount == 9
        then putStrLn "It's a tie! Type 'main' to play again."
    else do
        putStrLn $ " "
        renderBoard board
        putStrLn $ show player ++ " 's turn, type the space you'd like to select:"
        putStrLn $ " "
--Changing the user's input into an Int that could be used in later functions ended up being a challenge. I attempted to set it as input <- getChar then change the Char to Int, but it did not work. Finally, I discovered this method in a different piece of Haskell code and decided to try it.
        input <- readLn::IO Int
--Verifying that the input is valid (both the number not being higher than 9 and the space being open on the board) is obviously a priority in tic-tac-toe, and although this line correctly checks validity, the Exception throws an error before my 'else do' code block can run if the index is higher than 9. It looks like there might be a script that allows users to do just that (Control.Exception) but I have yet to test it due to wanting to keep the code as vanilla as possible.
        if cellIsOpen board input && input < 10 then do
            let newBoard = placePiece (input-1) (Occupied player) board
            if checkForWinner player newBoard then do
                putStrLn $ " "
                renderBoard newBoard
                putStrLn $ " "
                putStrLn $ show player ++ " is the Winner! Type 'main' to play again."
            else do
                runGame (changePlayers player) newBoard (guessCount + 1)
--This error code block runs if selected cell is not open or (supposedly) when the input is greater than 10.
        else do
            putStrLn $ " "
            putStrLn $ "ERROR: Entry not valid. Try again."
            putStrLn $ " "
            runGame player board (guessCount + 0)

--Moving the board to the 'main' portion was a last minute decision of style. Keeping the variable and it's assigned array above in the regular code block looked messy even though function did not seem to change in either location.
main :: IO ()
main = do
    putStrLn  "\n*~* Welcome to A 'Qwick' Game of Tic Tac Toe! *~*\n"
    runGame X board 0
        where board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]
# Tic-Tac-Toe With Haskell

For a fun game of Tic-Tac-Toe with a friend, your girlfriend, your grandmother, or your freakishly impressive dog, look no further than the easy-to-use Tic-Tac-Toe game featuring Haskell! This game was created with inspiration from Qwick and the functional programming languages they use as part of their platform's development.

This game can be ran and played completely from your computer's command line interface (such as your terminal)!

Creating this application was challenging because of the time frame I gave myself (3 days) and the added responsibility I had to learn a new language. Getting use to Haskell and aspects of functional programming in general was quite a bit to take in, especially in contrast to the cozy home I'd found within Javascript. Pushing myself to rapidly switch my coding paradigm ended up being a welcome challenge, one that has allowed me to walk into the job market confident that I can readily adapt to new technologies and languages.

For future development, I want to find a way or a third-party extension that would allow me to throw my own errors while maintaining the game's functionality. I would also like to explore an option to play with another partner or against the computer, necessitating that I implement AI features. I also began exploring some of the unit testing functionality available in Haskell, but chose not to implement it for this exercise since it involved importing packages outside of Data and System.IO.

![screenshot of the game](https://i.ibb.co/tDmwfMs/Haskell-screenshot.png)

## Install and Run

1. Install the [Haskell Toolchain](https://www.haskell.org/downloads/). If you are using a Mac, you can also install from your command line:
```
brew install haskell-platform
```
2. Download files to your computer; save the folder somewhere easily accessible (such as your Desktop.)
3. Open your computer's terminal and change directory into the saved folder. An example may be:
```
cd desktop
cd tic-tac-toe-main
```
4. Input the following commands to bring up the game.
```
ghci
:l tictactoe
main
```
## Playing the Game

1. The game will be played entirely through your computer's command line interface.
2. Player X will go first by default. When prompted, input your space selection 1-9.
3. After verifying your move is valid and applying to the board, player O will have a turn to select a space.
4. Gameplay will continue until someone wins the game or a tie occurs.
5. Having 3 of your pieces in a row diagonal, vertical, or horizonal results in you winning the game. A tie game occurs when all the pieces of the board are filled with no winner.
6. At the end of the game, type main to play again!
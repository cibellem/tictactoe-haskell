{-
Tic Tat Toe Game
Written in Haskell - Vanilla Haskell should be all you need but in that, you could include things from ‘Data’ and ‘System.IO’
Create a file - Create a file of your exercise that compiles. You can use any editor you prefer
Done in the CLI - All input/output should be done in the terminal. Display everything in the CLI
2 manual players - There’s no need to use AI, 2 humans (or animals) will be the ones to play the game
-}

--Cibelle MVP
-- When script starts users can choose to start or quit the game at any time
-- A visual representation of the board is displayed in the CLI
--

-- Good practice to use type declaration with our functions
-- doubleMe :: Int -> Int
-- doubleMe x = x + x

-- defines interfaces and behaviours of our data
type Turn = Int

type Board = String

type Option = Char

-- runGame is a recursive function that takes in the Board  and the players turn.
-- Recursion happens a lot in Haskell because there are no loops
-- A new board is generated after the player picks a position on the board.
runGame :: Board -> Turn -> IO ()
runGame slots turn = do
  if turn == 0
    then do
      putStrLn "Player X turn"
      putStrLn ("\n" ++ "" ++ show (slots !! 0) ++ "|" ++ show (slots !! 1) ++ "|" ++ show (slots !! 2) ++ "\n------------\n" ++ "" ++ show (slots !! 3) ++ "|" ++ show (slots !! 4) ++ "|" ++ show (slots !! 5) ++ "\n------------\n" ++ "" ++ show (slots !! 6) ++ "|" ++ show (slots !! 7) ++ "|" ++ show (slots !! 8) ++ "\n")
      pos <- getChar
      getChar -- By doing it the enter was not counted as an user input
      if notElem pos slots then do --the position is already taken we call runGame again with the same board
        putStrLn "Opsi option taken, try again!"
        runGame slots 0
        else runGame (newBoard slots turn pos) 1
    else do
      putStrLn "Player O turn"
      putStrLn ("\n" ++ "" ++ show (slots !! 0) ++ "|" ++ show (slots !! 1) ++ "|" ++ show (slots !! 2) ++ "\n------------\n" ++ "" ++ show (slots !! 3) ++ "|" ++ show (slots !! 4) ++ "|" ++ show (slots !! 5) ++ "\n------------\n" ++ "" ++ show (slots !! 6) ++ "|" ++ show (slots !! 7) ++ "|" ++ show (slots !! 8) ++ "\n")
      pos <- getChar
      getChar
      if notElem pos slots then do
        putStrLn "Opsi option taken, try again!"
        runGame slots 1
        else runGame (newBoard slots turn pos) 0

--Receives a board, a  player and a position on the board.
-- It returns a new board with the position
newBoard :: Board -> Turn -> Option -> Board
newBoard (x : xs) turn pos -- x is the first index of the list, the "head" xs is the rest of the list
  | (x == pos) && (turn == 0) = ['X'] ++ xs
  | (x == pos) && (turn == 1) = ['O'] ++ xs
  | otherwise = x : newBoard xs turn pos

-- The I/O action allow us to read inputs from the user and print things the screen.
-- Greets player and start/end game
main :: IO ()
main = do
  -- "do" glue together several I/O actions into one.
  putStrLn "Welcome! This is Tic Tac Toe built in Haskell :)\n" -- this is an I/O action.
  putStrLn "Press Y to start!\n"
  putStrLn "Type 'exit' at any time to quit.\n"
  input <- getLine -- "getLine" is an I/O action that contains a result type of String.   <- is used to bind the results of getline to names.
  if input == "exit"
    then return ()
    else runGame ['1', '2', '3', '4', '5', '6', '7', '8', '9'] 0 -- The very first game starts with player 0

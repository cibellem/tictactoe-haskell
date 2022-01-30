{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- defines interfaces and behaviours of our data
type Turn = Int

type Board = String

type Option = Char

--Player 0 -> X
--Player 1 -> O

-- runGame is a recursive function that takes in the Board and the Players turn.
-- Recursion happens a lot in Haskell because there are no loops
runGame :: Board -> Turn -> IO ()
runGame slots turn = do
  print slots
  if turn == 0
    then do
      putStrLn "Player X turn"
      putStrLn ("\n" ++ "" ++ show (slots !! 0) ++ "|" ++ show (slots !! 1) ++ "|" ++ show (slots !! 2) ++ "\n------------\n" ++ "" ++ show (slots !! 3) ++ "|" ++ show (slots !! 4) ++ "|" ++ show (slots !! 5) ++ "\n------------\n" ++ "" ++ show (slots !! 6) ++ "|" ++ show (slots !! 7) ++ "|" ++ show (slots !! 8) ++ "\n")
      pos <- getChar
      getChar -- By doing it the enter was not counted as an user input
      if notElem pos slots
        then do
          --the position is already taken we call runGame again with the same board
          putStrLn "Opsi option taken, try again!"
          runGame slots 0
        else runGame (newBoard slots turn pos) 1 -- the position is available we call new board to generate a new board. 1 is passed to indicate whose turn is on the next call to runGame
    else do
      putStrLn "Player O turn"
      putStrLn ("\n" ++ "" ++ show (slots !! 0) ++ "|" ++ show (slots !! 1) ++ "|" ++ show (slots !! 2) ++ "\n------------\n" ++ "" ++ show (slots !! 3) ++ "|" ++ show (slots !! 4) ++ "|" ++ show (slots !! 5) ++ "\n------------\n" ++ "" ++ show (slots !! 6) ++ "|" ++ show (slots !! 7) ++ "|" ++ show (slots !! 8) ++ "\n")
      pos <- getChar
      getChar
      if notElem pos slots
        then do
          putStrLn "Opsi option taken, try again!"
          runGame slots 1
        else runGame (newBoard slots turn pos) 0


--Receives a board a player's turn and a position on the board.
-- It returns a new board with the position
-- Here we use guards | and otherwise. They are a syntax sugar for if/else statements. Otherwise workds as a  "default" expression
-- if the 2 conditions are not met we call again newBoard but this time without the first index;
-- we recursively do it untill one of the conditions are met and then we generate a board with X or O applied to it.
newBoard :: Board -> Turn -> Option -> Board
newBoard (x : xs) turn pos -- x is the first index of the list xs is the rest of the list ['1', '2', '3', '4', '5', '6', '7', '8', '9']
 -- It reads like: Is the first index(head) of slots ['1', '2', '3', '4', '5', '6', '7', '8', '9'] in this case "1" equal to the value of pos and is the turn  equal to 0?
  | (x == pos) && (turn == 0)  = ['X'] ++ xs
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

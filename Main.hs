
-- This is a Haskell implementation of Tic Tac Toe ( PLayer Vs CPU )
--
-- The CPU uses a number of combinations to determine the winner and 
-- best combination is choosen by the CPU. The game if played properly
-- by the player either ends in tie or the CPU wins
--
-- Reference:
-- https://systematicgaming.wordpress.com/code-haskell-tic-tac-toe/


module Main where

type Board = [Char] -- list 

ticTacToe :: Board -> String
ticTacToe str =
  " " ++ str !! 0 : [] ++ " | " ++ str !! 1 : [] ++ " | " ++ str !! 2 : []  ++ " \n" ++
  "---+---+---\n" ++
  " " ++ str !! 3 : [] ++ " | " ++ str !! 4 : [] ++ " | " ++ str !! 5 : []  ++ " \n" ++
  "---+---+---\n" ++
  " " ++ str !! 6 : [] ++ " | " ++ str !! 7 : [] ++ " | " ++ str !! 8 : []  ++ " \n"

-- function :: validInput
-- Only integer inputs from 0 to 9 are accepted
validInput :: Char -> Bool
validInput c = filter (\x -> x == c) numbers /= []
  where
    numbers = [ '0' .. '9' ]

-- function :: positionValidity
-- this checks if the user input is between 0 - 9
positionValidity :: Board -> Int -> Bool
positionValidity board p
  | p < 0 || p >= 9           = False   -- out of range
  | validInput( board !! p )    = True    -- empty
  | otherwise                 = False   -- played

-- function :: validModes
-- Returns the list of valid moves for a given board
validMoves :: [Char] -> [Int]
validMoves board
  | (winner board) /= ' ' = []
  | otherwise = [y | y <- [0..8], (positionValidity board y)]

-- function :: move
-- Inputs are the board, a player and a move position, returns a new board with the
-- new move applied
move :: Board -> Char -> Int -> Board
move (p:board) ch pos
  | pos > 0 = p:[] ++ (move board ch (pos - 1))
  | otherwise = ch:[] ++ board

-- function :: scoreBoard
-- Returns 1 if the player is a winner, -1 if not and 0 if a tie
scoreBoard :: Board -> Char -> Int
scoreBoard board player
  | (winner board) == ' '     = 0 -- CPU is the winner
  | (winner board) == player  = 1 -- Player is the winner. very unlikely
  | otherwise                 = -1 -- Tie, no winner

-- function :: evaluateBoardMin
-- scores the board and returns minimum value move for the given board
evaluateBoardMin :: Board -> Int
evaluateBoardMin board
  | length (validMoves board) == 0    = scoreBoard board 'O'
  | otherwise = foldr max (head scores) (tail scores) -- get the first element and remove it
  where
  boards = map (move board 'O') (validMoves board)
  scores = map evaluateBoardMax boards

-- funtion :: evaluateBoardMax
-- scores the board and returns maximum value move for the given board
evaluateBoardMax :: Board -> Int
evaluateBoardMax board
  | length (validMoves board) == 0    = scoreBoard board 'O'
  | otherwise = foldr min (head scores) (tail scores) -- get the first element and remove it
  where
  boards = map (move board 'X') (validMoves board)
  scores = map evaluateBoardMin boards

-- function :: scoreMoves
-- Compute score for each possible move
-- Returns list of (Move, Score) tuples
scoreMoves :: Board -> [(Int, Int)]
scoreMoves board = zip (validMoves board) scores
  where
  boards = map (move board 'O') (validMoves board)
  scores = map evaluateBoardMax boards

-- function :: maxScore
-- returns the move with the highest score
maxScore :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxScore (m0, s0) (m1, s1)
  | s0 > s1 = (m0, s0)
  | otherwise = (m1, s1)

-- function :: bestMove
-- choose the best possible move for CPU
bestMove :: Board -> Int
bestMove board = move
  where
  scored = scoreMoves board
  (move, score) = foldr maxScore (head scored) (tail scored)

-- function :: playerMove
-- Attempts to make a move on the board
-- Returns (True, newBoard) if a valid move, with newBoard being a new game board
-- Returns (False, board) if an invalid move, with the board being unchanged
playerMove :: Board -> Int -> (Bool, Board)
playerMove board pos
  | not (positionValidity board pos) = (False, board)
  | otherwise = (True, (move board 'X' pos))

-- funtion :: winner
-- Checks if the board has a winning player
-- Returns '' if no winner, or the winner ('X' or 'O')
winner :: Board -> Char
winner b
  -- horizontal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 1) && (b !! 0) == (b !! 2)) = b !! 0
  | (b !! 3) /= ' ' && ((b !! 3) == (b !! 4) && (b !! 3) == (b !! 5)) = b !! 3
  | (b !! 6) /= ' ' && ((b !! 6) == (b !! 7) && (b !! 6) == (b !! 8)) = b !! 6
  -- vertical lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 3) && (b !! 0) == (b !! 6)) = b !! 0
  | (b !! 1) /= ' ' && ((b !! 1) == (b !! 4) && (b !! 1) == (b !! 7)) = b !! 1
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 5) && (b !! 2) == (b !! 8)) = b !! 2
  -- diagonal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 4) && (b !! 0) == (b !! 8)) = b !! 0
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 4) && (b !! 2) == (b !! 6)) = b !! 2
  -- no winner
  | otherwise = ' '


-- this is a recursive call that only terminates if one of the player is winner,
-- or the game ends in a tie.
-- If the game ends in a tie, then it runs into an error while printing the possible
-- winning combinations.
play :: Board -> IO () -- parameter is a function call
play board = do
  putStrLn ( ticTacToe board ) -- print the board
  if (length (validMoves board) == 0 || (winner board) /= ' ') -- checks if the user move is valid or no winner yet
    then do
      putStrLn("Winner " ++ (show (winner board) )); -- if winner, print the winner information
    else do
      putStrLn ( show (validMoves board) ) -- show the new board with the filled in spots
      putStrLn "Please choose a position. "
      pos <- getLine
      let (valid, b) = (playerMove board (read pos) ) -- check if the input is valid and not filled already
      if (valid) -- then CPU also plays
        then do putStrLn("\n CPU turn\n");
                if (' ' /= (winner b)) -- if the game is not tied and there still are winning combinations
                  then do -- print the winner board
                    putStrLn("Winner " ++ (show (winner b) ));
                    putStrLn ( ticTacToe b )
                  else do
                    putStrLn( show (scoreMoves b) ) -- show possible moves by the CPU, 0 is tie, 1 is winner, -1 is looser position for 'O'
                    if ( [] == (scoreMoves b)) -- if the best move list is empty, the game is a tie
                     then do
                      putStrLn("Game was a tie");
                     else do
                      putStrLn("Best move for CPU is position: " ++ show (bestMove b) ++ "\n" );
                    play (move b 'O' (bestMove b))     -- recursive funtion call                                  
        else do putStrLn("\nInvalid Move!\n");
                play board  

main = do
  putStrLn "------- Tic Tac Toe! -------\n You are X in this game.\n"
  play "012345678"

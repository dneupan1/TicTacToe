
-- https://systematicgaming.wordpress.com/code-haskell-tic-tac-toe/

module Main where

type Board = [Char]

showBoard :: Board -> String
showBoard str =
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
-- Given a board, a player and a move position, returns a new board with the
-- new move applied
move :: Board -> Char -> Int -> Board
move (p:board) ch pos
  | pos > 0 = p:[] ++ (move board ch (pos - 1))
  | otherwise = ch:[] ++ board

-- function :: scoreBoard
-- Score of a board (for our min/max tree)
-- Returns 1 if the player is a winner, -1 if not and 0 if a tie
scoreBoard :: Board -> Char -> Int
scoreBoard board player
  | (winner board) == ' '     = 0 -- CPU
  | (winner board) == player  = 1 -- Player
  | otherwise                 = -1 -- Tie

-- evaluateBoardMin
-- scores the board and returns minimum value move for the given board
evaluateBoardMin :: Board -> Int
evaluateBoardMin board
  | length (validMoves board) == 0    = scoreBoard board 'O'
  | otherwise = foldr max (head scores) (tail scores)
  where
  boards = map (move board 'O') (validMoves board)
  scores = map evaluateBoardMax boards

-- evaluateBoardMax
-- scores the board and returns maximum value move for the given board
evaluateBoardMax :: Board -> Int
evaluateBoardMax board
  | length (validMoves board) == 0    = scoreBoard board 'O'
  | otherwise = foldr min (head scores) (tail scores)
  where
  boards = map (move board 'X') (validMoves board)
  scores = map evaluateBoardMin boards

-- scoreMoves
-- Compute score for each possible move
-- Returns list of (Move, Score) tuples
scoreMoves :: Board -> [(Int, Int)]
scoreMoves board = zip (validMoves board) scores
  where
  boards = map (move board 'O') (validMoves board)
  scores = map evaluateBoardMax boards

-- maxScore
-- returns the move with the highest score
maxScore :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxScore (m0, s0) (m1, s1)
  | s0 > s1 = (m0, s0)
  | otherwise = (m1, s1)

-- bestMove
-- choose the best possible move for CPU
bestMove :: Board -> Int
bestMove board = move
  where
  scored = scoreMoves board
  (move, score) = foldr maxScore (head scored) (tail scored)

-- playerMove
-- Attempts to make a move on the board
-- Returns (True, newBoard) if a valid move, with newBoard being a new game board
-- Returns (False, board) if an invalid move, with the board being unchanged
playerMove :: Board -> Int -> (Bool, Board)
playerMove board pos
  | not (positionValidity board pos) = (False, board)
  | otherwise = (True, (move board 'X' pos))

-- winner
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

play :: Board -> IO ()
play board = do
  putStrLn ( showBoard board )
  if (length (validMoves board) == 0 || (winner board) /= ' ')
    then do
      putStrLn("Winner " ++ (show (winner board) ));
    else do
      putStrLn ( show (validMoves board) )
      putStrLn "Play? "
      pos <- getLine
      let (valid, b) = (playerMove board (read pos) )
      if (valid)
        then do putStrLn("\nOk\n");
                if (' ' /= (winner b))
                  then do
                    putStrLn("Winner " ++ (show (winner b) ));
                    putStrLn ( showBoard b )
                  else do
                    putStrLn( show (scoreMoves b) )
                    if ( null (show (bestMove b)))
                     then do
                      putStrLn("Game was a tie");
                     else do
                      putStrLn("Best move for CPU is position: " ++ show (bestMove b) ++ "\n" );                      
                    play (move b 'O' (bestMove b))
        else do putStrLn("\nInvalid Move!\n");
                play board  

main = do
  putStrLn "Tic Tac Toe!\n"
  play "012345678"



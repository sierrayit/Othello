module Othello where

import Data.Map as Map
import Data.List as List

data Color = White | Black | Empty deriving (Eq, Show)
data Direction = N | NE | E | SE | S | SW | W | NW deriving (Enum, Eq)
type Position = (Int, Int)
type Board = Map.Map Position Color

-- | A list of every position on the board.
everyPosition :: [Position]
everyPosition = [(x,y) | x <- [0..7], y <- [0..7]]

emptyBoard :: Board
emptyBoard = Map.fromList (List.zip everyPosition (repeat Empty))

-- | Board with the four initial pieces. 
freshBoard :: Board
freshBoard = Map.union (fromList (List.zip [(3,3), (3,4), (4,4), (4,3)] 
	[White, Black, White, Black])) emptyBoard

-- | Returns a list of positions to check in a line.
lineToCheck :: Position -> Direction -> [Position]
lineToCheck (x,y) dir
  | dir == N =  [(x, y+h)   | h <- [1..7], y+h <=7]
  | dir == E =  [(x+h, y)   | h <- [1..7], x+h <=7]
  | dir == S =  [(x, y-h)   | h <- [1..7], y-h >=0]
  | dir == W =  [(x-h, y)   | h <- [1..7], x-h >=0]
  | dir == NE = [(x+h, y+h) | h <- [1..7], x+h <=7, y+h <=7]
  | dir == SE = [(x+h, y-h) | h <- [1..7], x+h <=7, y-h >=0]
  | dir == SW = [(x-h, y-h) | h <- [1..7], x-h >=0, y-h >=0]
  | otherwise = [(x-h, y+h) | h <- [1..7], x-h >=0, y+h <=7]
 
-- | Returns true if the move causes an opponents piece to be flipped and the position is empty. 
isLegalMove :: Board -> Color -> Position -> Bool
isLegalMove b c p = (Map.!) b p == Empty && List.length (piecesToFlip b p c) > 0

-- | Returns a list of valid moves for a color.
getAvailableMoves :: Board -> Color -> [Position]
getAvailableMoves b c = List.filter (\p -> isLegalMove b c p) everyPosition

-- | Returns a list of pieces that need to be flipped in a line. 
piecesToFlip :: Board -> Position -> Color -> [Position]
piecesToFlip b p c = List.concat 
	[toFlipInLine b c l | l <- List.map (lineToCheck p) [N .. NW]]

-- | Returns a list of colors at each position passed in.
color :: Board -> [Position] -> [Color]
color b = List.map (b Map.!)

-- | Flips the color if the color isn't empty. 
flipColor :: Color -> Color
flipColor Black = White
flipColor White = Black
flipColor Empty = Empty

-- | Finds the list of positions whose color needs to be flipped.
toFlipInLine :: Board -> Color -> [Position] -> [Position]
toFlipInLine _ _ [] = []
toFlipInLine _ _ (_:[]) = []
toFlipInLine b p pl 
	| beginning /= [] && fst (head beginning) == p = List.map snd toFlip
	| otherwise = []
	where 
		colorandpos = zip (color b pl) pl
		toFlip = List.takeWhile (\x -> fst x == flipColor p) colorandpos
		beginning = List.dropWhile (\x -> fst x == flipColor p) colorandpos  


makeMove :: Position -> Board -> Color -> Board
makeMove p b c = let pl = piecesToFlip b p c in
	Map.union (fromList [(p, c)]) 
		(Map.union (fromList $ List.zip pl (repeat $ c)) b) 

-- | Returns true when neither player can move.
gameFinished :: Board -> Bool
gameFinished b = not (any (isLegalMove b Black) everyPosition || 
					  any (isLegalMove b White) everyPosition)

-- | Counts the number of pieces a certain color has.
countColors :: Board -> Color -> Int
countColors b col = Map.fold 
		(\value acc -> if value == col then 1 + acc else acc) 0 b		

-- | Returns the color with the most pieces if the game is finished.
winner :: Board -> Color
winner b
	| gameFinished b =  if numB > numW then Black
					   	else if numB == numW then Empty
					    else White
	| otherwise = error "Game not finished"
		where 
			numB = countColors b Black
			numW = countColors b White

-- | Returns an integer value for the advantage of that board given a board, color and depth.
getAdvantage :: Int -> Board -> Color -> Double
getAdvantage depth board color =
	let
		legalC1Moves = getAvailableMoves board color
		legalC2Moves = getAvailableMoves board (flipColor color)
		nextColor = if legalC2Moves /= [] then flipColor color else color
		legalMovesForNextColor = if nextColor == color then legalC1Moves 
								 else legalC2Moves
		maxAdvForNextColor = maximum $
				List.map (\p -> getAdvantage 
					(depth - 1) (makeMove p board color) nextColor)
					legalMovesForNextColor
	in if gameFinished board
		then
			if winner board == color
				then 100000
				else -100000
		else
			if depth <= 0
				then advantageHeuristic board color
				else if nextColor /= color
					then - maxAdvForNextColor
					else maxAdvForNextColor

-- | calculates the advantage of this color with this board state
advantageHeuristic :: Board -> Color -> Double
advantageHeuristic b c = parity b c + 10 * mobility b c

-- | calculates the ratio of the difference of the number of pieces each player currently has
parity :: Board -> Color -> Double
parity b c = (c1 - c2) / (c1 + c2)
	where 
		c1 = fromIntegral $ countColors b c
		c2 = fromIntegral $ countColors b (flipColor c)

-- | calculates the ratio of the difference of the number of moves available to each player
mobility :: Board -> Color -> Double
mobility b c = (c1 - c2) / (c1 + c2)
	where
		c1 = fromIntegral . List.length $ getAvailableMoves b c
		c2 = fromIntegral . List.length $ getAvailableMoves b (flipColor c)

printColorz :: Color -> String
printColorz color =
	case color of
		Empty -> " "
		White -> "\ESC[38;5;15mW\ESC[0m"
		Black -> "\ESC[38;5;4mB\ESC[0m"

printRow :: Board -> Int -> String
printRow board row = show row ++ "|" ++
	(intercalate "|" (List.map
		(\position -> printColorz (board ! position))
		([(x, row) | x <- [0..7]]))) ++
	"|" ++ show row

printBoard :: Board -> String
printBoard board =
	"\n  0 1 2 3 4 5 6 7 \n ~*~*~*~*~*~*~*~*~\n" ++
	(intercalate
		"\n ~*~*~*~*~*~*~*~*~\n"
		(List.map
			(printRow board)
			$ List.reverse [0..7])) ++
	"\n ~*~*~*~*~*~*~*~*~\n  0 1 2 3 4 5 6 7 \n"

-- | gets the position of the available move with the highest calculated advantage
getAIMove :: Board -> Position
getAIMove board =
	(\(pos, _) -> pos)
		(List.maximumBy
			(\(_,advantage1) (_,advantage2) -> compare advantage1 advantage2)
			(List.map
				(\position -> (position, getAdvantage 2 
					(makeMove position board Black) Black))
				(getAvailableMoves board Black)))

-- | reads the input position from the command line
getUserMove :: Board -> Color -> IO Position
getUserMove board color = do
	l <- getLine
	return $ read l  

-- | handles each move, with input from the command line in the case of one player
gameLoop :: Board -> Color -> Int -> IO ()
gameLoop board color numPlayers = do
	if gameFinished board
		then
			do
			putStrLn $ printBoard board
			if gameWinner == Empty then putStrLn "Game Over: Tie"
				else if gameWinner == Black 
					then putStrLn "Game Over: Black Wins"
					else putStrLn "Game Over: White Wins"
		else
			do		
			putStrLn $ printBoard board
			if hasMove == White
				then
					do
					putStrLn "Please enter a move for the White player"
					--putStrLn $ List.concat $ List.map show (getAvailableMoves board color)
					wm <- whiteMove
					if isLegalMove board White wm
						then
							gameLoop (makeMove wm board White) 
								(flipColor color) numPlayers
						else
							do
							putStrLn "Illegal Move."
							gameLoop board color numPlayers
				else 
					if numPlayers == 1
						then
							do
							putStrLn 
							   "Entering AI generated move for the Black player"
							gameLoop (makeMove (getAIMove board) board Black) 
								(flipColor color) numPlayers
						else
							do
							putStrLn "Please enter a move for the Black player"
							bm <- blackMove
							if isLegalMove board Black bm
								then
									gameLoop (makeMove bm board Black) 
										(flipColor color) numPlayers
								else
									do
									putStrLn "Illegal Move."
									gameLoop board color numPlayers
	where
		gameWinner = winner board
		whiteMove = getUserMove board White
		hasMove = if (getAvailableMoves board color) /= [] 
				  then color 
				  else flipColor color
		blackMove = getUserMove board Black

-- | Starts the game by prompting the user for initial conditions and then starting the game loop
main :: IO ()
main = do
	putStrLn "Would you like to play 1 Player or 2 Player?"
	putStrLn "Please enter either '1' or '2'"
	playerNumber <- getLine
	if (read playerNumber /= 1 && read playerNumber /= 2) 
		then error "Invalid Number of Players."
		else gameLoop freshBoard White $ read playerNumber

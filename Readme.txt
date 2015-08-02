Othello
- Xander and Sierra

Description of your project and what you accomplished:
We implemented Othello, or Reversi, for play through the command line as either a two player or one player game. In order to do this we had to create multiple types to represent the pieces and the board, and then figure out the logic for game over conditions, as well as legal moves and flipping pieces. We accomplished a working command line Othello, with optional AI. 


Instructions on how to compile/run/try out/play with your project:
To compile:
ghc Othello.hs
To run:
./Othello
An ASCII representation of a new game will be printed to the terminal and the game will prompt you to choose whether you want to play with two players or one. You enter moves in the format (x,y), where x and y are ints between 0 and 7, corresponding to the rows and columns of the board. If you try and input an illegal move (see http://www.site-constructor.com/othello/othellorules.html for the rules of the game), the game will prompt you for a legal move. 

A description of work you did and things you learned along the way:
The work we did can be split up into categories; game logic, AI, standard IO. The game logic was actually more difficult to design than expected. We had to come up with a way to check if a move was valid, which meant that we had to check every possible line (ie. row, column diagonal) in both directions to see if the new move would cause an opponent's piece to be flipped. So we decided to create a type for direction, and then check every direction to get a list of pieces that would have to be flipped. When checking each direction the dropWhile and takeWhile functions were really helpful. The rest of the game logic was straightforward, we just had to check if the game was over when neither piece could move. 
Then the AI was the more interesting part, we had to learn how the minimax algorithm worked and then research a heuristic to use for when the depth of the minimax algorithm wasn't enough to reach an end game state. The heuristic we ended up choosing was based on both the number of pieces each player had on the board and the number of moves they could make from that position. We weighted the number of moves higher than the number of pieces because in Othello it's very easy to switch the number of pieces in one move. This was probably the coolest part of the assignment. 
The finishing piece of the game was the game loop in which we had to deal with input from the command line. This was probably where we spent the most time. At first we wanted to be able to handle the inputs by reading in the arguments from the command line into the maybe monad, and therefore check if there was a valid input. Then we wanted to check if it was a valid input by using a regex comparison. Neither of these options worked, so we ended up using read to get the tuple of coordinates from the command line. The other slightly tricky thing about the game loop was all of the do notation for each expression that used putStrLn.
Overall we learned a lot about IO and the minimax algorithm. 




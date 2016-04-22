#| ########################################################## 
                  ***** OTHELLO.LSP *****
                  
                 Command line reversy game
########################################################## |#

;------------- load files -------------
(load 'minimax)
(load 'display-positions.lsp)

;required empty constructor
(defun othello-init ()
    
)



#| ########################################################## 
        **print-position**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (print-position position)
          where position is the position to be evaluated.
          
Description: Displays a formated game state
########################################################## |#
(defun print-position (position)
    (let ((i 0))
	    (format t "~%")
		(format t "  1 2 3 4 5 6 7 8~%")
        (dotimes (j 8)
		    (format t "~S" (1+ j))
            (dotimes (k 8)
			    (format t " ")
		        (prin1 (nth i position))
				(setf i (1+ i))
		    )
			(format t "~%")
        )
	)
)



#| ##########################################################
        **do-move**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (do-move position player row col)
          where position is the position to be evaluated,
          player is the current color, and row and col 
          are where the next position is being updated
          
Returns;  newPosition - updated position after move
          nil - move was invalid
          
Description: updates the game mode for a move at the
             given cordinants
########################################################## |#
(defun do-move (position player row col)
    (let ((newPosition nil)
	      (tempPosition nil)
		  (i nil))
		  
		; check if we went off the board
		(when (or (< row 0)
				  (> row 7)
				  (< col 0)
				  (> col 7))
			(return-from do-move nil)
		)
		(setf i (+ (* row 8) col))
		(when (not (equal (nth i position) '-))
		    (return-from do-move nil)
		)
		
		;check each direction
		(dotimes (direction 8)
		    (if (null newPosition)
			    (setf tempPosition (check-direction position player row col direction))
				(setf tempPosition (check-direction newPosition player row col direction))
			)
			(if (not (null tempPosition))
			    (setf newPosition tempPosition)
			)
		)
		;return
		newPosition
	)
)



#| ########################################################## 
        **make-move-human**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (make-move-human position player)
          where position is the position to be evaluated,
          and player is the current color.
          
Returns;  newPosition - updated position after move

Functions called:
          (do-move position player row col) -
            used to update the board with move
          (print-position position) -
            formates a position to be displayed
          (display-positions position player)
            updates the board to display + where the
            player has a move
          
Description: catches input from a user and uses it to
             make a move on the board
########################################################## |#
(defun make-move-human (position player)
    (let ((newPosition nil))
        (print-position (display-positions position player) )
	    (loop while (null newPosition) do
		    (if (equal player 'B)
			    (format t "~%Black's turn.~%")
				(format t "~%White's turn.~%")
			)
		    (format t "What is your move [row col]? ")
			(let* ((row (read))
			       (col (read)))
				(setf newPosition (do-move position player (1- row) (1- col)))
				(when (null newPosition)
				    (format t "~%Move (~S, ~S) is invalid!~%" row col)
				)
				(when (not (null newPosition))
				    (format t "~%")
				)
			)
		)
		;return
		newPosition
	)
)



#| ##########################################################
        **make-move**

Author: Johnny Ackerman
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (make-move position player ply)
          where position is the position to be evaluated,
          player is the current color, and ply is the depth
          to be searched to.
          
Returns;  newPosition - updated position after move

Functions called:
          (minimax position depth color alpha beta is-max) -
            determines the move for the AI element
          (print-position position) -
            formates a position to be displayed
          
Description: Calls minimax to have the AI make a move
######################################################## |#
(defun make-move (position player ply)
    ;ai should make its move
	;setup, then call minimax
    (let (row column answer)
         (print-position position)
         (setf answer ;(minimax state depth color alpha beta is-max)
               (minimax position ply player -10000 10000 t)
         )
         (setf row (nth 1 (nth 0 (cadr answer))))
         (setf column (nth 2 (nth 0 (cadr answer))))
		 (format t "~%Here is my move: ~S ~S~%" (1+ row) (1+ column))
		 ;return board state after move was made
		 (nth 0 (nth 0 (cadr answer)))
    )
)



#| ##########################################################
        **can-move**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (can-move boardState color)
          where boardState is the position to be evaluated,
          color is the current player.
          
Functions called:
          (check-direction boardState color row col j)
            looks for a move in a given direction
          
Returns;  t - found a move
          nil - did not find a move

          
Description: checks if a move is possible
######################################################## |#
(defun can-move (boardState color)
    ;basically generate-successors
	;except return after one move is found
    (let ((succ nil)
	      (row nil)
          (col nil))
        ; check each position
        (dotimes (i 64)
            (when (equal (nth i boardState) '-)
                (setf row (floor i 8))
                (setf col (- i (* row 8)))
                ; check each direction
                (dotimes (j 8)
                    (setf succ (check-direction boardState
                                                color
                                                row
                                                col
                                                j))
                    (when succ
                        ;found a move
						(return-from can-move t)
                    )
                )
            )
        )
        
        ; didn't find a move
        nil
    )
)



#| ##########################################################
        **game-over**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (game-over state)
          where state is the position to be evaluated,
          color is the current player.
          
Functions called:
          (print-position position) -
            formates a position to be displayed
          
Description: Determines and displays a winner and exits
             the game
######################################################## |#
(defun game-over (state)
    ;count score and output score and who won
	(let ((countBlack 0)
	      (countWhite 0))
		; check each position
        (dotimes (i 64)
            (when (equal (nth i state) 'B)
                (setf countBlack (1+ countBlack))
            )
			(when (equal (nth i state) 'W)
			    (setf countWhite (1+ countWhite))
			)
        )
		
		(print-position state)
		(format t "~%Game Over!~%")
		(format t "~%Black: ~S~%White: ~S~%~%" countBlack countWhite)
		(if (= countBlack countWhite)
		    (format t "Tie Game!~%")
			(if (> countBlack countWhite)
		        (format t "Black Wins!~%")
			    (format t "White Wins!~%")
		    )
		)
		
		
	)
)



#| ##########################################################
        **start-game**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (start-game player)
          where player is repesentative of the human player
          
Functions called:
          (can-move boardState color)
            checks if player can move
          (make-move-human boardState turn)
            gets the human's move
          (make-move boardState turn ply)
            gets the AI's move
          
          
Description: runs the Othello game vs AI
######################################################## |#
(defun start-game (player)
    (format t "~%OK! You will be playing ")
    (if (eq player 'B)
	    (format t "Black. ")
		(format t "White. ")
	)
    (format t "When asked for you move, please enter the row and column in which you would like to place a ")
	(if (eq player 'B)
	    (format t "Black ")
		(format t "White ")
	)
	(format t "stone. Remember, you must outflank at least one ")
	(if (eq player 'B)
	    (format t "White ")
		(format t "Black ")
	)
	(format t "stone, or forfeit your move.~%")
	(let ((turn 'B)
	      (gameOver nil)
		  (playerMoved t)
		  (boardState nil))
		  
		(setf boardState '(- - - - - - - -
                      - - - - - - - -
                      - - - - - - - -
                      - - - W B - - -
                      - - - B W - - -
                      - - - - - - - -
                      - - - - - - - -
                      - - - - - - - -) )
		
		;start game loop
		(loop 
		    while (null gameOver) do
		    ;check if player can't move
			(cond
			    ((null (can-move boardState turn))
				    (when (null playerMoved)
					    ;last player could not move either
						;game over
						(game-over boardState)
						(setf gameOver t)
					)
					(when playerMoved
					    ;can't move but other player might be able to
						(format t "~%No moves available. Turn is forfeit!!!~%")
					    (setf playerMoved nil)
					)
			    )
				(t
				    ;reset flag that is used to
					;check if both players can't move
				    (setf playerMoved t)
				    ;check if players turn
					(when (eq player turn)
					    ;human's turn
						(setf boardState (make-move-human boardState turn))
					)
					(when (not (eq player turn))
					    ;AI's turn
					    (setf boardState (make-move boardState turn 4))
					)
				)
			)
			;other players turn now
		    (if (eq turn 'B)
			    (setf turn 'W)
				(setf turn 'B)
			)
		)
	)
)



#| ##########################################################
        **othello-two-player**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (othello-two-player)
          
Functions called:
          (can-move boardState color)
            checks if player can move
          (make-move-human boardState turn)
            gets the human's move
          
          
Description: runs the Othello game for two humans
######################################################## |#
(defun othello-two-player ()
    (format t "When asked for you move, please enter the row and column in which you would like to place a ")
	(format t "stone. Remember, you must outflank at least one of your opponents")
	(format t "stones, or forfeit your move.~%")
	(let ((player 'B)
	      (turn 'B)
	      (gameOver nil)
		  (playerMoved t)
		  (boardState nil))
		  
		(setf boardState '(- - - - - - - -
                      - - - - - - - -
                      - - - - - - - -
                      - - - W B - - -
                      - - - B W - - -
                      - - - - - - - -
                      - - - - - - - -
                      - - - - - - - -) )
		
		;start game loop
		(loop 
		    while (null gameOver) do
		    ;check if player can't move
			(cond
			    ((null (can-move boardState turn))
				    (when (null playerMoved)
					    ;last player could not move either
						;game over
						(game-over boardState)
						(setf gameOver t)
					)
					(when playerMoved
					    ;can't move but other player might be able to
						(format t "~%No moves available. Turn is forfeit!!!~%")
					    (setf playerMoved nil)
					)
			    )
				(t
				    (setf playerMoved t)
				    ;check if players turn
					(when (eq player turn)
					    ;human's turn
						(setf boardState (make-move-human boardState turn))
					)
					(when (not (eq player turn))
					    ;other human's turn
					    (setf boardState (make-move-human boardState turn))
					)
				)
			)
			;other players turn now
		    (if (eq turn 'B)
			    (setf turn 'W)
				(setf turn 'B)
			)
		)
	)
)


#| ##########################################################
        **othello**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (othello <player>)
          where player is the color that the user wants to be
          
Functions called:
          (start-game color)
            starts the othello game
          
          
Description: determines the color of the human when
             playing an AI
######################################################## |#
(defun othello (&optional player)
    (cond
	    ((equalp player "Black")
	        (start-game 'B)
		)
		((equalp player "White")
		    (start-game 'W)
		)
		(t 
		    ;get input from player
			(format t "Would you like to move first [y/n]? ")
			(let (userInput (read-line))
			    (when (null userInput)
				    (setf userInput (read-line))
				)
                (cond
				    ((or (equal userInput "y")
					     (equal userInput "Y"))
				        (start-game 'B)
					)
					(t
					    (start-game 'W)
					)
				)			
			)
		)
	)
)


;---------- code ----------
(if
    ;test for command line argument
	;if one argument then pass to (othello) function
	(= (length *ARGS*) 1)
	(othello (first *ARGS*))
)

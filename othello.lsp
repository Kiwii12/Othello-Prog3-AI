;------------- load files -------------
(load 'generate-successors)
(load 'minimax)
(load 'test-list)

(defun othello-init ()
    
)

(defun make-move-human (position player)
)

(defun make-move (position player ply)
    
)

(defun can-move ()
)

(defun game-over (state)
)

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
	(format t "stone, for forfeit your move.~%")
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
		(while (null gameOver)
		    ;check if player can't move
			(cond
			    ((null (can-move turn))
				    (when (null playerMoved)
					    ;last player could not move either
						;game over
						(game-over boaredState)
						(setf gameOver t)
					)
					(when playerMoved
					    ;can't move but other player might be able to
					    (setf playerMoved nil)
					)
			    )
				(t
				    ;check if players turn
					(when (eq player turn)
					    ;human's turn
						(make-move-human boardState turn)
					)
					(when (not (eq player turn))
					    ;AI's turn
					    (make-move boardState turn 4)
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
    ;test for argument
	(= (length *ARGS*) 1)
	(othello (first *ARGS*))
)

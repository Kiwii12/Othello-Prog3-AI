;------------- load files -------------
(load 'minimax)
(load 'test-list)

(defun othello-init ()
    
)

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

;execute a player entered move
;return board after move has been made
;return nil if invalid move
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
			    (setf tempPosition (check-direction-validate-move position player row col direction))
				(setf tempPosition (check-direction-validate-move newPosition player row col direction))
			)
			(if (not (null tempPosition))
			    (setf newPosition tempPosition)
			)
		)
		;return
		newPosition
	)
)


(defun make-move-human (position player)
    (let ((newPosition nil))
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
					(print-position newPosition)
				)
			)
		)
		;return
		newPosition
	)
)


(defun make-move (position player ply)
    ;ai should make its move
	;setup, then call minimax
    (let (row column answer newPosition)
         ;(print-position position)
         (setf answer (minimax position ply player))
         (setf row (nth 1 (nth 0 (cadr answer))))
         (setf column (nth 2 (nth 0 (cadr answer))))
         (setf newPosition (do-move position player row column))
         (print-position newPosition)
         newPosition
    )
)

(defun can-move (boardState color)
    ;basically generate-successors
	;except return after one move is found
    (let ((succ nil)
	      (row nil)
          (col nil))
        ; check each position
        (dotimes (i 64)
            (when (equal (nth i boardState) color)
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
		
		;(print-position state)
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
		;display starting board
		(print-position boardState)
		
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
					    (setf playerMoved nil)
					)
			    )
				(t
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
		;display starting board
		(print-position boardState)
		
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

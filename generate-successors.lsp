#| ########################################################## 
                  ***** GENERATE-SUCCESSOR.LSP *****
                  
                generates all possible moves of a depth
########################################################## |#



#| ########################################################## 
        **generate-successors**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (generate-successors position color)
          where position is the position to be evaluated,
          and color is the player who makes the turn.
          
Returns;  ((board-list) row col) - boardstate with cordinates
                    of the next move (returns list of this object)

Functions called:
          (check-direction position color row col direction) -
            determines if a move is valid and gets its cordinants
          
Description: generates possible moves and makes a list of said
             moves with the coordinants required to get there
########################################################## |#
(defun generate-successors (position color)
    (let ((successors nil)
          (row nil)
          (col nil))
        ; check each position
        (dotimes (i 64)
            (when (equal (nth i position) '-)
                (setf row (floor i 8))
                (setf col (- i (* row 8)))
				
				(let ((succ nil)
				      (tempPosition nil))
				    ;check each direction
					(dotimes (direction 8)
						(if (null succ)
							(setf tempPosition (check-direction position color row col direction))
							(setf tempPosition (check-direction succ color row col direction))
						)
						(if (not (null tempPosition))
							(setf succ tempPosition)
						)
					)
					
					(when succ
						; add succ to successors list 
						(setf successors (append successors (list(list succ row col))))
					)
				)
				
				
            )
        )
        
        ; return successors
        successors
    )
)



#| ##########################################################
        **check-direction**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (check-direction positions color row col direction)
          where position is the state being evaluated,
          color is the current player being evaluated, row and col,
          are the current spot in question, and direction which is
          the path of (going to be flipped) tiles.
          
Returns:  (succ row col)
          new position with the row and column to get it
          nil - not a valid move/direction
            
          
Description: get a new board and coordinats to get it
######################################################## |#
(defun check-direction (position color row col direction)
    (let ((succ (copy-list position))
           (i nil)
           (spot nil)
		   (rowOld row)
		   (colOld col)
           (firstLoop t))
		   
		; check if we went off the board
		(when (or (< row 0)
				  (> row 7)
				  (< col 0)
				  (> col 7))
			(return-from check-direction nil)
		)
		(setf i (+ (* row 8) col))
		(setf (nth i succ) color)
        
        (loop
            ; look at adjacent spot
            (cond
                ((= direction 0) (setf row (1+ row)))
                ((= direction 1) (setf row (1+ row) col (1+ col)))
                ((= direction 2) (setf col (1+ col)))
                ((= direction 3) (setf row (1- row) col (1+ col)))
                ((= direction 4) (setf row (1- row)))
                ((= direction 5) (setf row (1- row) col (1- col)))
                ((= direction 6) (setf col (1- col)))
                ((= direction 7) (setf row (1+ row) col (1- col)))
                (t nil)
            )
            ; check if we went off the board
            (when (or (< row 0)
                      (> row 7)
                      (< col 0)
                      (> col 7))
                (return-from check-direction nil)
            )
            (setf i (+ (* row 8) col))
            (setf spot (nth i position))
			(setf (nth i succ) color)
			
			; this gets skipped on the first loop
			(when (not firstLoop)
			    ; if spot is same color as player return succ
				(when (equal spot color)
				    (return-from check-direction succ)
				)
				; if spot is empty return nil
				(when (equal spot '-)
					(return-from check-direction nil)
				)
			)
			
            ; return if first spot in direction is not opponents
			; this only gets checked on the first loop
            (when firstLoop
                (setf firstLoop nil)
                (when (or (equal spot color)
                          (equal spot '-))
                    (return-from check-direction nil)
                )
            )
			
        )
      
    )
)



#| ##########################################################
        **check-direction-validate-move**

Author: Jacob St.Amand
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (check-direction positions color row col direction)
          where position is the state being evaluated,
          color is the current player being evaluated, row and col,
          are the current spot in question, and direction which is
          the path of (going to be flipped) tiles.
          
Returns:  succ - new position 
          nil - invalid
          
Description: gets a board state for a move if valid
             similar to check-direction, except used to validate and
			 execute a move made by human player
######################################################## |#
(defun check-direction-validate-move (position color row col direction)
    (let ((succ (copy-list position))
           (i nil)
           (spot nil)
		   (rowOld row)
		   (colOld col)
           (firstLoop t))
		   
		; check if we went off the board
		(when (or (< row 0)
				  (> row 7)
				  (< col 0)
				  (> col 7))
			(return-from check-direction-validate-move nil)
		)
		(setf i (+ (* row 8) col))
		(setf (nth i succ) color)
        
        (loop
            ; look at adjacent spot
            (cond
                ((= direction 0) (setf row (1+ row)))
                ((= direction 1) (setf row (1+ row) col (1+ col)))
                ((= direction 2) (setf col (1+ col)))
                ((= direction 3) (setf row (1- row) col (1+ col)))
                ((= direction 4) (setf row (1- row)))
                ((= direction 5) (setf row (1- row) col (1- col)))
                ((= direction 6) (setf col (1- col)))
                ((= direction 7) (setf row (1+ row) col (1- col)))
                (t nil)
            )
            ; check if we went off the board
            (when (or (< row 0)
                      (> row 7)
                      (< col 0)
                      (> col 7))
                (return-from check-direction-validate-move nil)
            )
            (setf i (+ (* row 8) col))
            (setf spot (nth i position))
			(setf (nth i succ) color)
			
			; this gets skipped on the first loop
			(when (not firstLoop)
			    ; if spot is same color as player return succ
				(when (equal spot color)
				    (return-from check-direction-validate-move succ)
				)
				; if spot is empty return nil
				(when (equal spot '-)
					(return-from check-direction-validate-move nil)
				)
			)
			
            ; return if first spot in direction is not opponents
			; this only gets checked on the first loop
            (when firstLoop
                (setf firstLoop nil)
                (when (or (equal spot color)
                          (equal spot '-))
                    (return-from check-direction-validate-move nil)
                )
            )
			
        )
      
    )
)
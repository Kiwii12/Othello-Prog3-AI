#| ########################################################## 
                  ***** DISPLAY-POSITIONS.LSP *****
                  
                      displays moves to choose
########################################################## |#



#| ##########################################################
        **display-positions**

Author: Johnny Ackerman (based on Jacobs code in generate-successors)
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (display-positions positions color)
          where position is the state being evaluated, and
          color is the current player being evaluated.
          
Returns:  new-position
          positions with + for valid player moves
          
Functions called:
          (valid-move-locations position color row col j) -
            checks if a move is valid and gets its position
          
          
Description: displays help for humans to make moves
######################################################## |#
(defun display-positions (position color)
    (let ((new-position (copy-list position))
          (succ nil)
          (row nil)
          (col nil))
        ; check each position
        (dotimes (i 64)
            (when (equal (nth i position) color)
                (setf row (floor i 8))
                (setf col (- i (* row 8)))
                ; check each direction
                (dotimes (j 8)
                    (setf succ (valid-move-location position
                                                color
                                                row
                                                col
                                                j))
                    (when succ
                        ; add succ to successors list 
                        (setf (nth (cadr succ) new-position) '+ )
                    )
                )
            )
        )
        
        ; return count
        new-position
    )
)




#| ##########################################################
        **valid-move-location**

Author: Johnny Ackerman (based on Jacobs code in generate-successors)
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (valid-move-location position color row col direction)
          where position is the state being evaluated,
          color is the current player being evaluated, row and col,
          are the current spot in question, and direction which is
          the path of (going to be flipped) tiles.
          
Returns:  (t i) - valid move and where to make it
          nil - invalid move
          
          
          
Description: checks if a move can be done and returns its
             cordinates
######################################################## |#
(defun valid-move-location (position color row col direction)
    (let (
           (i nil)
           (spot nil)
		   (rowOld row)
		   (colOld col)
           (firstLoop t))
        
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
                (return-from valid-move-location nil)
            )
            (setf i (+ (* row 8) col))
            (setf spot (nth i position))
			
			; this gets skipped on the first loop
			(when (not firstLoop)
			    ; if spot is same color as player return nil
				(when (equal spot color)
				    (return-from valid-move-location nil)
				)
				; if spot is empty return true
				(when (equal spot '-)
					(return-from valid-move-location (list t i))
				)
			)
			
            ; return if first spot in direction is not opponents
			; this only gets checked on the first loop
            (when firstLoop
                (setf firstLoop nil)
                (when (or (equal spot color)
                          (equal spot '-))
                    (return-from valid-move-location nil)
                )
            )
			
        )
      
    )
)
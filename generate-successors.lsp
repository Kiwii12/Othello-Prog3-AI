
; returns a list of ((board-list) row col)
(defun generate-successors (position color)
    (let ((successors nil)
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
                    (setf succ (check-direction position
                                                color
                                                row
                                                col
                                                j))
                    (when succ
                        ; add succ to successors list 
                        (setf successors (append successors (list (list succ row col))))
                    )
                )
            )
        )
        
        ; return successors
        successors
    )
)


(defun check-direction (position color row col direction)
    (let ((succ (copy-list position))
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
                (return-from check-direction nil)
            )
            (setf i (+ (* row 8) col))
            (setf spot (nth i position))
			(setf (nth i succ) color)
			
			; this gets skipped on the first loop
			(when (not firstLoop)
			    ; if spot is same color as player return nil
				(when (equal spot color)
				    (return-from check-direction nil)
				)
				; if spot is empty return succ
				(when (equal spot '-)
					(return-from check-direction succ)
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
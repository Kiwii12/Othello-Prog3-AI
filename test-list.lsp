(setf start-list '(- - - - - - - -
                   - - - - - - - -
                   - - - - - - - -
                   - - - W B - - -
                   - - - B W - - -
                   - - - - - - - -
                   - - - - - - - -
                   - - - - - - - -) )

(setf corner-list '(B - - - - - - -
                    - B - W W W - -
                    - - B W W W - -
                    - - W B W W - -
                    - W W W B W - -
                    - W W W W B - -
                    - - - - - - - -
                    - - - - - - - -) )
					
(defun print-position (position)
    (let ((i 0))
	    (format t "~%")
        (dotimes (j 8)
            (dotimes (k 8)
			    (format t " ")
		        (prin1 (nth i position))
				(setf i (1+ i))
		    )
			(format t "~%")
        )
	)
)

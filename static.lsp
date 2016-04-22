
(setf *IS-AI-1* t)

(defun static (position color)
    (if *IS-AI-1*
	
		(let ( (s-value 0))
			 ;board is 8 by 8
			 
			 ;corners are highly s-valued
			 ;spot 0, 7, 56, and 63
			 (if (equal (nth 0  position) color) (setf s-value (+ s-value 500)))
			 (if (equal (nth 7  position) color) (setf s-value (+ s-value 500)))
			 (if (equal (nth 56 position) color) (setf s-value (+ s-value 500)))
			 (if (equal (nth 63 position) color) (setf s-value (+ s-value 500)))
			 
			 
			 
			 ;it turns out that going for the least (especially at the begining)
			 ;helps maintain having a lot of moves and limits opponents moves
			 (dolist (element position)
				(if (equal element color) (setf s-value (- s-value 1)))    
			 )

			 
			 
			 ;force s-value to be the return
			 s-value
		)
		(let ( (s-value 0))
			 ;board is 8 by 8
			 
			 ;corners are highly s-valued
			 ;spot 0, 7, 56, and 63
			 (if (equal (nth 0  position) color) (setf s-value (- s-value 500)))
			 (if (equal (nth 7  position) color) (setf s-value (- s-value 500)))
			 (if (equal (nth 56 position) color) (setf s-value (- s-value 500)))
			 (if (equal (nth 63 position) color) (setf s-value (- s-value 500)))
			 
			 
			 
			 ;it turns out that going for the least (especially at the begining)
			 ;helps maintain having a lot of moves and limits opponents moves
			 (dolist (element position)
				(if (equal element color) (setf s-value (- s-value 1)))    
			 )

			 
			 
			 ;force s-value to be the return
			 s-value
		)
	)
)






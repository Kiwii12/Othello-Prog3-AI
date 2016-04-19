(defun static (position color)
    (let ( (value 0) )
         ;board is 8 by 8
         
         ;corners are highly valued
         ;spot 0, 7, 56, and 63
         (if (equal (nth 0  position) color) (setf value (+ value 200)))
         (if (equal (nth 7  position) color) (setf value (+ value 200)))
         (if (equal (nth 56 position) color) (setf value (+ value 200)))
         (if (equal (nth 63 position) color) (setf value (+ value 200)))
         
         ;spaces next to the corners are bad
         ;spot 1, 6, 8, 9, 14, 15, 48, 49, 54, 55, 57, and 62 
         (if (equal (nth 1  position) color) (setf value (- value 50)))  
         (if (equal (nth 6  position) color) (setf value (- value 50)))
         (if (equal (nth 8  position) color) (setf value (- value 50)))
         (if (equal (nth 9  position) color) (setf value (- value 50)))
         (if (equal (nth 14 position) color) (setf value (- value 50)))
         (if (equal (nth 15 position) color) (setf value (- value 50)))
         (if (equal (nth 48 position) color) (setf value (- value 50)))
         (if (equal (nth 49 position) color) (setf value (- value 50)))
         (if (equal (nth 54 position) color) (setf value (- value 50)))
         (if (equal (nth 55 position) color) (setf value (- value 50)))
         (if (equal (nth 57 position) color) (setf value (- value 50)))
         (if (equal (nth 62 position) color) (setf value (- value 50)))
         
         
         ;keep a bit of focus on the center
         (if (equal (nth 27 position) color) (setf value (+ value 15)))
         (if (equal (nth 28 position) color) (setf value (+ value 15)))
         (if (equal (nth 35 position) color) (setf value (+ value 15)))
         (if (equal (nth 36 position) color) (setf value (+ value 15)))
         
         ;its good to keep the spaces outside of the "sweet sixteen"
         (if (equal (nth 2  position) color) (setf value (+ value 2)))
         (if (equal (nth 5  position) color) (setf value (+ value 2)))
         (if (equal (nth 10 position) color) (setf value (+ value 2)))
         (if (equal (nth 13 position) color) (setf value (+ value 2)))
         (if (equal (nth 16 position) color) (setf value (+ value 2)))
         (if (equal (nth 17 position) color) (setf value (+ value 2)))
         (if (equal (nth 18 position) color) (setf value (+ value 2)))
         (if (equal (nth 21 position) color) (setf value (+ value 2)))
         (if (equal (nth 22 position) color) (setf value (+ value 2)))
         (if (equal (nth 23 position) color) (setf value (+ value 2)))
         (if (equal (nth 40 position) color) (setf value (+ value 2)))
         (if (equal (nth 41 position) color) (setf value (+ value 2)))
         (if (equal (nth 42 position) color) (setf value (+ value 2)))
         (if (equal (nth 45 position) color) (setf value (+ value 2)))
         (if (equal (nth 46 position) color) (setf value (+ value 2)))
         (if (equal (nth 47 position) color) (setf value (+ value 2)))
         (if (equal (nth 50 position) color) (setf value (+ value 2)))
         (if (equal (nth 53 position) color) (setf value (+ value 2)))
         (if (equal (nth 58 position) color) (setf value (+ value 2)))
         (if (equal (nth 61 position) color) (setf value (+ value 2)))
         
         (dolist (element position)
            (if (equal element color) (setf value (- value 1)))    
         )
         
         ;force value to be the return
         value
    )
)
#| ########################################################## 
                  ***** STATIC.LSP *****
                  
             determins the value of positions
########################################################## |#

(load 'check-moves.lsp          )
(load 'swap-color.lsp           )



#| ##########################################################
        **static**

Author: Johnny Ackerman
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (static postion color)
          where position is the state being evaluated, and
          color is the current player being evaluated.
          
Returns:  s-value
          where s-value is the evaluations of the position
          
Functions called:
          
          
Description: evaluates board state
######################################################## |#
(defun static (position color)
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
)
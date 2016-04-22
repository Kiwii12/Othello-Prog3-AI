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
          
Returns:  value
          where value is the evaluations of the position
          
Functions called:
          (check-moves position color) -
            checks for an amount of moves
          (swap-color color) -
            changes to opponents color
          
          
Description: determines the color of the human when
             playing an AI
######################################################## |#
(defun static (position color)
    (let ( (value 0) (anti-color (swap-color color)) (empty-spaces 0))
         ;board is 8 by 8
         
		 ;corners are highly valued
		 ;spot 0, 7, 56, and 63
		 (if (equal (nth 0  position) color) (setf value (+ value 500)))
		 (if (equal (nth 7  position) color) (setf value (+ value 500)))
		 (if (equal (nth 56 position) color) (setf value (+ value 500)))
		 (if (equal (nth 63 position) color) (setf value (+ value 500)))
		 
		 
		 
		 ;it turns out that going for the least (especially at the begining)
		 ;helps maintain having a lot of moves and limits opponents moves
		 (dolist (element position)
			(if (equal element color) (setf value (- value 1)))    
		 )

		 
		 
         ;force value to be the return
         value
    )
)
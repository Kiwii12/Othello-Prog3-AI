#| ########################################################## 
                  ***** SWAP-COLOR.LSP *****
                  
                         change color
########################################################## |#

#| ##########################################################
        **swap-color**

Author: Johnny Ackerman
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (swap-color color)
          where color is the current player
          
Returns:  color
          color is the opposite players color
                    
          
Description: changes the opposite color
######################################################## |#
(defun swap-color
    (color)
    (let ()
        (if (equal color 'B) 
            (setf color 'W);then 
            (setf color 'B);else
        )
        color
    )
)
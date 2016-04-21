#| ########################################################## 
                  ***** DEEP-ENOUGH.LSP *****
                  
                       are we there yet
########################################################## |#


#| ##########################################################
        **swap-color**

Author: Johnny Ackerman
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (deepenough color)
          where color is the current player
          
Returns:  t - is actually deep enough
          nil - not yet
                    
          
Description: checks if the value is 0 or less
######################################################## |#
(defun deepenough
    (depth)
    (let ()
        (<= depth 0) 
    )
)
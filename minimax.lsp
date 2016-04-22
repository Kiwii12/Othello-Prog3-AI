#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth color alpha beta is-max)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).
          alpha - beta should be set to  -infinity and 
          +infinity respectivly for pruning to work 
          properly. Is-max should be set to t from the intial
          call as well.

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (generate-successors position color) -
              generates successors to the position.

          (static position color) -
              applies the static evaluation function to the position.
              
          (swap-color color) -
              switches to the opposing color.

          Note: these functions may need additional arguments.
          
Modifications:
    The function was adapted to do alpha beta pruning.
    Many of the function calls were given the option argument color
        so that they could work for our function calls.
    A second let statement was added to catch the value from the first
        generate-successors call to improve efficiency.
|#

(load 'static.lsp               )
(load 'generate-successors.lsp  )
(load 'deep-enough.lsp          )
(load 'swap-color.lsp           )

(defun minimax (position depth color alpha beta is-max)

    ;outer let to store successors so that they are only generated once
    (let (successors)
    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
        (if (or (deepenough depth) ; generate list of sucessor positions
                (null (setf successors (generate-successors position color)))
            )
			;swap-color because previous color made the move that generated this position
			;negative because the value will be negated as it is passed/returned to the parent node
            (list (- (static position (swap-color color))) nil)

            ; otherwise, generate successors and run minimax recursively
            (let
                (
                    ; initialize current best path to nil
                    (best-path nil)

                    ; initialize current best score to negative infinity
                    (best-score -1000000)

                    ; other local variables
                    succ-value
                    succ-score
                )

                ; explore possible moves by looping through successor positions
                (dolist (successor successors)

                    ; perform recursive DFS exploration of game tree
                    (setq succ-value 
                          (minimax (car successor) 
                                   (1- depth) 
                                   (swap-color color)
                                   alpha beta
                                   (not is-max) 
                          )
                    )

                    ; change sign every ply to reflect alternating selection
                    ; of MAX/MIN player (maximum/minimum value)
                    (setq succ-score (- (car succ-value)))

                    ;set new alpha beta values
                    (if is-max (when (< alpha succ-score) 
                                   (setf alpha succ-score) 
                               )
                               (when (> beta  (- succ-score))
                                   (setf beta  (- succ-score))
                               )
                    )


                    ; update best value and path if a better move is found
                    ; (note that path is being stored in reverse order)
                    (when (> succ-score best-score)
                          (setq best-score succ-score)
                          (setq best-path (cons successor (cdr succ-value)))
                    )
					
					;cut out if needed
                    (when (<= beta alpha) 
                                   ;(format t  
                                   (return) 
                    )
					
                )

                ; return (value path) list when done
                (list best-score best-path)
            )
        )
    )
)




;literally minimax with a different static because pakages are dumb
(defun minimax-2 (position depth color alpha beta is-max)

    ;outer let to store successors so that they are only generated once
    (let (successors)
    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
        (if (or (deepenough depth) ; generate list of sucessor positions
                (null (setf successors (generate-successors position color)))
            )
			;swap-color because previous color made the move that generated this position
			;negative because the value will be negated as it is passed/returned to the parent node
            (list (- (static-2 position (swap-color color))) nil)

            ; otherwise, generate successors and run minimax recursively
            (let
                (
                    ; initialize current best path to nil
                    (best-path nil)

                    ; initialize current best score to negative infinity
                    (best-score -1000000)

                    ; other local variables
                    succ-value
                    succ-score
                )

                ; explore possible moves by looping through successor positions
                (dolist (successor successors)

                    ; perform recursive DFS exploration of game tree
                    (setq succ-value 
                          (minimax-2 (car successor) 
                                   (1- depth) 
                                   (swap-color color)
                                   alpha beta
                                   (not is-max) 
                          )
                    )

                    ; change sign every ply to reflect alternating selection
                    ; of MAX/MIN player (maximum/minimum value)
                    (setq succ-score (- (car succ-value)))

                    ;set new alpha beta values
                    (if is-max (when (< alpha succ-score) 
                                   (setf alpha succ-score) 
                               )
                               (when (> beta  (- succ-score))
                                   (setf beta  (- succ-score))
                               )
                    )


                    ; update best value and path if a better move is found
                    ; (note that path is being stored in reverse order)
                    (when (> succ-score best-score)
                          (setq best-score succ-score)
                          (setq best-path (cons successor (cdr succ-value)))
                    )
					
					;cut out if needed
                    (when (<= beta alpha) 
                                   ;(format t  
                                   (return) 
                    )
					
                )

                ; return (value path) list when done
                (list best-score best-path)
            )
        )
    )
)
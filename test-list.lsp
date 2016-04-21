#| ########################################################## 
                  ***** TEST-LIST.LSP *****
                  
                         sample data
########################################################## |#


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

(setf endgame-list '(B B B B B B W W
                     B B B W W W W B
                     W B B W W W W W
                     W B W B W W W W
                     W W W W B W B W
                     B W W W W B B W
                     B W W W W B B W
                     B W W W W B B W) )
					
;black can move, white can not
(setf test-list1 '(B B B B B B W W
                   B B B W W W W -
                   W B B W W W W W
                   W B W B W W W W
                   W W W W B W B W
                   B W W W W B B W
                   B W W W W B B W
                   B W W W W B B W) )
					
;either color can move
(setf test-list2 '(B B B B B B W W
                   - B B W W W W B
                   W B B W W W W W
                   W B W B W W W W
                   W W W W B W B W
                   W W W W W B B W
                   B W W W W B B W
                   B W W W W B B W) )
					
;white can move, black can not
(setf test-list3 '(B B B B B B W -
                   B B B W W W W B
                   W B B W W B B B
                   W B W B B B B B
                   W W B B B B B B
                   W W B B B B B B
                   W W B B B B W B
                   W W B B B B B B) )
					
(setf test-list4 '(B - - - - - - B
                   - - - - - - - -
                   - - - - - - - -
                   - - - - - - - -
                   - - - - - - - -
                   - - - - - - - -
                   - - - - - - - -
                   B - - - - - - W) )
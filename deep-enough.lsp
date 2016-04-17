(if (equal (boundp '*ply*) nil) (setf *ply* 2))

(defun deepenough
    (depth)
    (let ()
        (>= depth *ply*) 
    )
)
;----------------------------------------------------------------------------------------------------------------------------------
;;from http://common-lisp.net/project/elephant/darcs/stdutils/matrix.lisp
(defun invert-matrix (matrix &optional (destructive T))
 "Find the inverse of a matrix.  By default this operation is
 destructive.  If you want to preserve the original matrix, call this
 function with an argument of NIL to destructive."
 (let ((result (if destructive matrix (copy-matrix matrix)))
       (size (array-dimension matrix 0))
       (temp 0))
   (dotimes (i size result)
     (setf temp (aref result i i))
     (dotimes (j size)
       (setf (aref result i j)
             (if (= i j)
                 (if (=(aref result i j) 0) 0 (/ (aref result i j)) )
                 (if (= temp 0) 0 (/ (aref result i j) temp))               )))
     (dotimes (j size)
       (unless (= i j)
         (setf temp (aref result j i)
               (aref result j i) 0)
         (dotimes (k size)
           (setf (aref result j k)
                 (- (aref result j k)
                    (* temp (aref result i k))))))))))
;----------------------------------------------------------------------------------------------------------------------------------
;;from http://rosettacode.org/wiki/Matrix_multiplication#Common_Lisp
(defun matrix-multiply (a b)
 (flet ((col (mat i) (mapcar #'(lambda (row) (elt row i)) mat))
        (row (mat i) (elt mat i)))
   (loop for row from 0 below (length a)
         collect (loop for col from 0 below (length (row b 0))
                       collect (apply #'+ (mapcar #'* (row a row)
(col b col)))))))
;-----------------------------------------------------------------------------------------------------------------------------
;;from http://aleteia.wikidot.com/lisp:derivada
(defun deriv (fn)
 (let ((dx 1/1000000000))
   #'(lambda (x)
       (/ (- (funcall fn (+ x dx)) (funcall fn x))
          dx))))
	
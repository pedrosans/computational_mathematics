;http://en.wikipedia.org/wiki/Gauss%E2%80%93Seidel_method
;(load "C:/java/git-repo/computational_mathematics/util.lisp")
;(load "C:/java/git-repo/computational_mathematics/math_util.lisp")
(load "C:/dev/git-repo/computational_mathematics/util.lisp")
(load "C:/dev/git-repo/computational_mathematics/math_util.lisp")
(setq f1 '(+ (* 4 x1) (* x2 -1) (* x3 -1) 1))
(setq f2 '(+ (* -1 x1) (* 4 x2 ) (* -1 x3) (* -1 x4) -2))
(setq f3 '(+ (* -1 x1) (* -1 x2) (* 4 x3) (* -1 x4) (* -1 x5) -6 ))
(setq f4 '(+ (* -1 x2) (* -1 x3) (* 4 x4) (* -1 x5) -2))
(setq f5 '(+ (* -1 x3) (* -1 x4 ) (* 4 x5) 1))
;(setq x1 1)(setq x2 2)(setq x3 3)(setq x4 4)(setq x5 5)
(setq x1 0)(setq x2 0)(setq x3 0)(setq x4 0)(setq x5 0)
(defun constant(equation variables)
	(eval (replace_elements equation variables 0))
)
(defun multiplier (equation variable variables)
	(setq C (constant equation variables))
	(setq aux (replace_elements equation (set-difference variables (list variable)) 0))
	(setq aux (replace_elements aux (list variable) 1))
	(-(eval aux) C)
)
(defun lower-diagonal (matrix)
	(setq result (list))
	(dotimes (i (list-length matrix))
		(setq line (list))
		(dotimes (j (list-length (nth i matrix)))
			(setq line (append line (list(if (> j i ) 0 (nth j (nth i matrix)) ))))
		)
		(setq result (append result (list line)))
	)
	result
)
(defun higher-diagonal (matrix)
	(setq result (list))
	(dotimes (i (list-length matrix))
		(setq line (list))
		(dotimes (j (list-length (nth i matrix)))
			(setq line (append line (list(if (<= j i ) 0 (nth j (nth i matrix)) ))))
		)
		(setq result (append result (list line)))
	)
	result
)
(defun operate_matrix(m1 m2 f)
	(setq result (list))
	(dotimes (i (list-length m1))
		(setq line (list))
		(dotimes (j (list-length (nth i m1)))
			(setq line (append line (list 
				(funcall f (nth j (nth i m1))(if (< j (list-length(nth i m2))) (nth j (nth i m2)) 0 ))
			)))
		)
		(setq result (append result (list line)))
	)
	result
)
(defun matrix_A ( equations variables)
	(setq A (list))
	(mapcar (lambda(equation)
		(setq C (constant equation variables))
		(setq A_line (list))
		(mapcar (lambda (variable) 
			(setq A_line (append A_line (list (multiplier equation variable variables)))) 
		) variables)
		(setq A (append A (list A_line)))
	) equations)
	A
)
(defun matrix_B( equations variables)
	(setq B (list))
	(mapcar (lambda(equation)
		(setq B (append B (list(list (* -1(constant equation variables))))))
	) equations)
	B
)
(defun matrix_X(variables)
	(setq X (list))
	(mapcar (lambda (var)(setq X (append X (list (list (SYMBOL-VALUE var)))))) variables)
	X
)
(defun gauss_seidel(equations variables precision iteration )
	;Ax = b
	(setq b (matrix_B equations variables))
	(setq A (matrix_A equations variables))
	(setq X (matrix_X variables))
	;A = L + U
	(setq L (lower-diagonal A))
	(setq U (higher-diagonal A))
	;Lx = b - Ux
	(setq array_L (to_array L))
	(invert-matrix array_L )
	(setq L_invertido (to_list array_L)) 
	(setq Ux (matrix-multiply U X))
	(setq Xk (matrix-multiply L_invertido (operate_matrix b Ux #'-)))
	(setq delta (list))
	(dotimes (i (list-length Xk))
		(print i)
		(setq delta (append delta (list (- (car (nth i Xk)) (car (nth i X)) ) )))
	)
	(print_list delta)
)
(gauss_seidel (list f1 f2 f3 f4 f5) '(x1 x2 x3 x4 x5) 1/100000 10)
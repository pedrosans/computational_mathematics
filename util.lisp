;;    util.lisp - simple set of utility methods
;;    Copyright (C) 2011 Pedro Henrique Oliveira dos Santos
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;    Contact: pedrosans at gmail dot com
;;
(defun print_matrix(m)(format t "~%") (mapcar (lambda(i)( mapcar(lambda (j)(format t "~,10F  " j )) i ) (format t "~%") ) m))
(defun print_list(m) (format t "~%")(mapcar(lambda (j)(format t "~,10F " (if (symbolp j)(eval j) j) )) m ))
;-----------------------------------------------------------------------------------------------------------------------------
;;
;; :equacao lista de simbolos que representam a equação, cuidar para não passar uma função como parâmetro
;; :target o símbolo da equação representando a variável referencia
;; :vars a lista de todas variáveis que existem na função
;;
(defun derivada_parcial(equacao target vars)
	(lambda ()
		(funcall
			(deriv
				(lambda (xl)
					(setq lamb xl)
					(eval (replace_elements (replace_elements equacao (set-difference vars (list target)) 1) (list target) 'lamb) )
				)
			) (eval target)
		)
	)
)
;-----------------------------------------------------------------------------------------------------------------------------
;;
;; Retorna a matriz de funções para calcular a matriz jacobiana
;;
(defun jacobiana(funcoes variaveis &key (J (list)))
	(progn
		(loop for f in funcoes  do
			(setq J_line (list))
			(loop for x in variaveis do
				(setq J_line (append J_line  (list (derivada_parcial f x variaveis))))
			)
			(setq J (append J  (list J_line)))
		)
       J
	)
)
;-----------------------------------------------------------------------------------------------------------------------------
(defun replace_elements(list_p target replacement)
	(cond
		((eq list_p nil) nil)
		((listp (car list_p)) (cons (replace_elements (car list_p) target replacement) (replace_elements (cdr list_p) target replacement)))
		((not (eq (intersection (list(car list_p)) target) nil)) (cons replacement (replace_elements (cdr list_p) target replacement)))
		(t (cons (car list_p) (replace_elements (cdr list_p) target replacement)))
	)
)
;-----------------------------------------------------------------------------------------------------------------------------
(defun to_array(target_list &key (i 0) (a (make-array (list-length target_list))))
	(loop for e in target_list  do
		(if (listp e)
			(setf (aref a i) (to_array e))
			(setf (aref a i) e)
		)
		(setq i (+ i 1))
	)
	a
)
;-----------------------------------------------------------------------------------------------------------------------------
(defun to_array(target_list &key (i 0) (a (make-array (list
(list-length target_list) (list-length target_list)))))
       (loop for r in target_list  do
               (setq j 0)
               (loop for c in r do
                       (setf (aref a i j) c)
                       (setq j (+ j 1))
               )
               (setq i (+ i 1))
       )
       a
)
;-----------------------------------------------------------------------------------------------------------------------------
(defun to_list(matrix &key (response (list)))
       (loop for i from 0 to (-(array-dimension matrix 0) 1) do
               (setq line (list))
               (loop for j from 0 to (-(array-dimension matrix 1) 1) do
                       (setq line (append line (list (aref matrix i j) )))
               )
               (setq response (append response (list line)))
       )
       response
)
;-----------------------------------------------------------------------------------------------------------------------------
(defun find_max(lista &key(m (car lista)))
   (cond
		((eq (car lista) nil) m )
		((listp m)(find_max lista :m (car m)))
		((listp (car lista)) (setq m(find_max (car lista) :m m)) (find_max (cdr lista) :m m))
		(t (progn (if (> (car lista) m) (setq m (car lista)))(find_max (cdr lista) :m m)))
	)
)
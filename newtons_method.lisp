;;    newtons_method.lisp - Newton's method to solve nonlinear equations
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

;;Importante: alterar o caminho dos arquivos utilit�rios. Ex.: c:/src/util.lisp

(load "C:/java/git-repo/computational_mathematics/util.lisp")
(load "C:/java/git-repo/computational_mathematics/math_util.lisp")

;-----------------------------------------------------------------------------------------------------------------------------
;;
;; :funcoes lista de fun��es do sistema de equa��es
;; :variaveis lista dos s�mbolos representando as vari�veis das equa��es
;; :precisao almejada
;; :repeticoes n�mero m�ximo de intera��es
;;
(defun newton (funcoes variaveis precisao repeticoes &key( MJ (jacobiana funcoes variaveis) ))
	(if (= repeticoes 0) 
		 variaveis 
		(progn
			(format t "~% Chute na interacao ~D" repeticoes)
			(print_list variaveis)
					 
			(setq j (mapcar (lambda(x)( mapcar(lambda (y) (funcall y )) x ))MJ))
			(setq fx (mapcar (lambda(f) (list (eval f))  ) funcoes ))
			(setq fx (mapcar (lambda(i)( mapcar(lambda (j)(* j -1)) i ) ) fx))

			(setq array_J (to_array j))
			(invert-matrix array_J )
			(setq j_invertido (to_list array_J))

			(setq delta (matrix-multiply j_invertido fx))
			
			(setq i 0)(mapcar (lambda(variacao)(setf (SYMBOL-VALUE(nth i variaveis)) (+(car variacao) (eval (nth i variaveis))) ) (setq i (+ i 1))) delta )
			
			(setq d (mapcar (lambda(f) (eval f) ) funcoes ))
			(setq d (mapcar (lambda(f) (if (>= f 0) f(* f -1)) ) d ))
						
			(if (> precisao (find_max d))
				(progn (print "precisao encontrada") variaveis)
				(newton funcoes variaveis precisao (- repeticoes 1) :MJ MJ)
			)
		)
	)
)

(setq f1 '(+ (/ (+ (* 2 x1) (* -1 (expt x1 2)) 8) 9 )  (/ (+ (* 4 x2)(* -1 (expt x2 2))) 4 )  ))
(setq f2 '(+ (* 8 x1) (* -4 (expt x1 2)) (expt x2 2) 1 ))
(setq x1 -1)
(setq x2 -1)
(setq resultado (newton (list f1 f2  ) '(x1 x2  ) 1/10000 5))
(print "resultado: ")(print_list resultado)
;;
;;exemplo de utiliza��o:
;;
;;definindo as equa��es
;(setq f1 '(+ (* x1 x1 ) (* x2 x2) (* x3 x3) -1))
;(setq f2 '(+ (* 2 x1 x1) (* x2 x2) (* -4 x3 )))
;(setq f3 '(+ (* 3 x1 x1) (* -4 x2) (* x3 x3 )))
;; chute inicial
;(setq x1 5)
;(setq x2 23)
;(setq x3 6)
;;imprime resultados
;(setq resultado (newton (list f1 f2 f3) '(x1 x2 x3) 1/10000 5))
;(print "resultado: ")(print_list resultado)

;-----------------------------------------------------------------------------------------------------------------------------
(defun find_root(funcao derivada chute precisao interacoes)
	(if (< interacoes 0) 
		chute
		(progn 
			(setq sombra (f chute))
			(format t "~,10F ~,10F ~D ~%" chute sombra interacoes)
			(if (and (< sombra precisao) (> sombra (* precisao -1)))
				(progn (print "precisao alcancada") chute )
				(find_root funcao derivada (- chute (/ (funcall funcao chute) (funcall derivada chute)) ) precisao (- interacoes 1))
			)
		)
	)
)
;;exemplo de utiliza��o:
;	(defun f(x) (+ (* x x x) (* x x -5) x 3))
;	(defun d(x) (+ (* x x) (* x -0) 1))
;	(print (find_root #'f #'d  0.5 0.00001 20))

;;    regula_falsi.lisp - find root for given function
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

;;
;; :x1 :x2 intervalo em que a raiz vai ser procurada
;;
(defun regula_falsi(f x1 x2 precisao interacoes)
	(if (eq interacoes 0) 
		x1
		(progn 
			(setq sombra (f x1))
			(format t "~%~,10F	~,10F ~,10F ~D" x1 x2 sombra interacoes)
			(if (and (< sombra precisao) (> sombra (* precisao -1)))
				(progn (print "precisao alcancada") chute )
				(progn 
					;encontra a raiz da secante entre os pontos do intervalo x1 x2
					(setq chute (/(-(*(read-from-string (format NIL "~20,20F" x1))(funcall f x2)) (* x2 (funcall f x1)))(-(funcall f x2)(funcall f x1))) )
					(if (> (f chute) 0) (setq x1 chute) (setq x2 chute))
					(regula_falsi f x1 x2 precisao (- interacoes 1))
				)
			)
		)
	)
)
; exemplo de utilização
;(defun f(x)(+ (* x x x) (* -9 x ) 3 ))
;(print (regula_falsi #'f 0 1 0.00001 20 ))
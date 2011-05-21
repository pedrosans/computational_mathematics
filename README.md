Computational Mathematics
=========================

Overview
--------
Set of algorithms to solve commom mathematical problems and utility methods.

Newton Raphson method
---------------------

### To solve nonlinerar equations
In the newtons_method.lisp file there are the algorithm to solve nonlinear equations using Newton an Rapson's method.

In order to use the newton function you need to define the set of equations:

	(setq f1 '(+ (* x1 x1 ) (* x2 x2) (* x3 x3) -1))
	(setq f2 '(+ (* 2 x1 x1) (* x2 x2) (* -4 x3 )))
	(setq f3 '(+ (* 3 x1 x1) (* -4 x2) (* x3 x3 )))

Give the initial guess:

	(setq x1 1/2)
	(setq x2 1/2)
	(setq x3 1/2)

The method expects a list of equations, variables, the precistion and the maximum iterations number. The result is returned as a list. 
	
	(newton (list f1 f2 f3) '(x1 x2 x3) 1/10000 5)
	

### To find root
In the newtons_method.lisp file there are the algorithm to find the root of some equation.

The method find_root expects the function, its derivate, the initial guess, the precision and maximun iterations number.

	(defun f(x) (+ (* x x x) (* x x -5) x 3))
	(defun d(x) (+ (* x x) (* x -0) 1))
	(print (find_root #'f #'d  0.5 0.00001 20))

Regula Falsi method
-------------------

### To find the function root

The method regula_falsi in regula_falsi.lisp file expects the function, the interval of the root, precision, and the maximun iterations number.

	(defun f(x)(+ (* x x x) (* -9 x ) 3 ))
	(print (regula_falsi #'f 0 1 0.00001 20 ))
	
Secant method
-------------

### To find the funciton root

The method secant in secant.lisp file expects the function, 2 initial guesses, precision, and the maximun iterations number.

	(defun f(x)(+ (* x x x) (* x x -1) (* -12 x ) ))
	(print (secant #'f 2 3 0.00001 20 ))
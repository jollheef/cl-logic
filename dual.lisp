;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

;;; Двойственная функция

(defun map-recursive (func list)
  (loop for i in list collect
       (if (listp i)
	   (map-recursive func i)
	   (apply func (list i)))))

(defun dual (listfunc)
  (map-recursive
   (lambda (x) (cond ((equal x '∧) '∨)
		((equal x '∨) '∧)
		((equal x '↑) '↓)
		((equal x '↓) '↑)
		((equal x '⊕) '∼)
		((equal x '∼) '⊕)
		((equal x 't) 'nil)
		((equal (string x) "NIL") 't)
		(t x)))
   listfunc))

(defun dual-infix (listfunc)
  (dual (infix->prefix listfunc)))

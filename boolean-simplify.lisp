;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package :cl-logic)

(defun boolean-terms (vector)
  (remove nil (loop for i in vector for n from 0 collect (if i n))))

(defun boolean-simplify-raw (function)
  (quine-mccluskey:quine-mccluskey
   (concatenate
    'string
    (loop for i from 0 repeat (nvars function)
       collect (char+ #\a i)))
   (boolean-terms (result-vector function))))

(defun convert-raw-string-to-boolean (raw-string)
  (append '(∧) (loop for i across raw-string
		  for n from 0 collect
		    (let ((var (intern (string (char+ #\a n)))))
		      (cond ((equalp i #\1) var)
			    ((equalp i #\0) (list '¬ var)))))))

(defun convert-raw-list-to-boolean (raw-list)
  (loop for disj in raw-list
     collect (remove nil (convert-raw-string-to-boolean disj))))

(defun boolean-simplify (function)
  (prefix->infix
   (append '(∨) (convert-raw-list-to-boolean
		 (nth-value 1 (boolean-simplify-raw function))))))

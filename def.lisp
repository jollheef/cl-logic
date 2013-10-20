;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

;; Определение булевых функций

(defun symbol-lessp (symbol1 symbol2)
  (string-lessp (string symbol1) (string symbol2)))

(defun get-symbols (list)
  (loop for seq in (cdr list) collect
       (if (listp seq)
	   (get-symbols seq)
	   seq)))

(defun get-args (list)
  (let ((args-list '()))
    (defun args (lst)
      (loop for i in lst do
	   (if (listp i)
	       (args i)
	       (if (not (or (equal i 't) (equal i 'nil)))
		   (push i args-list)))))
    (args (get-symbols list))
    (sort (remove-duplicates args-list) 'symbol-lessp)))

(defmacro def-infix (name body)
  (let ((lfunc (infix->prefix body)))
    `(defun ,name ,(get-args lfunc) ,lfunc)))

(defmacro def (name body)
  `(defun ,name ,(get-args body) ,body))

(defmacro def-dual-infix (name body)
  (let ((lfunc (dual-infix body)))
    `(defun ,name ,(get-args lfunc) ,lfunc)))

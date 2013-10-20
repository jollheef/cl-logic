;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

(defvar *separators* '(↓ ↑ ∼ ⊕ → ∨ ∧ ¬)
  "Операторы по убыванию приоритета.") 

(defun remove-brackets (lst)
  "Reduses lists with just one item to the item itself"
  (do ((result lst (car result)))
      ((or (not (consp result))
	   (not (null (cdr result)))) result)))

(defun separate-list (lst separator test)
  "Returns list of sub-sequences defined by separator"
  (if (not (consp lst))
      lst
      (let ((result (cons separator nil)) (end 0) (sub)
	    (lst (if (funcall test (car lst) separator)
		     (cdr lst)
		     lst)))
	(do () ((null lst) result)
	  (setf end 
		(position separator lst :test test))
	  (setf sub
		(cons (subseq lst 0 end) nil))
	  (setf result 
		(append result sub))
	  (setf lst 
		(if end 
		    (nthcdr (+ 1 end) lst)
		    nil)))
	(setf (cdr result) (mapcar #'remove-brackets (cdr result)))
	result)))

(defun separate-tree (lst separator test)
  "Apply separate-list on all sublists"
  (if (or (not (consp lst)) (eql (first lst) 'quote))
      lst
      (progn
	(setf lst (mapcar #'(lambda (x) 
			      (if (not (consp x))
				  x
				  (separate-tree x separator test)))
			  lst))
	(if (not (find separator (rest lst)))
	    lst
	    (separate-list lst separator test)))))

(defun infix->prefix-optimized (infix-expr
				&optional (separators *separators*)
				  (test #'eql))
  "Переходит от инфиксной записи к префиксной"
  (let ((result infix-expr))
    (dolist (sep separators)
      (setf result (separate-tree result sep test)))
    (remove-brackets result)))

(defun only-two-arg (prefix-expression)
  (defun op2 (lst func)
    "Преобразует список вида '(+ 1 2 3 4) в '(+ 1 (+ 2 (+ 3 4)))"
    (setf lst (map 'list
		   (lambda (l) (if (and l (listp l))
			      (op2 (cdr l) (car l))
			      l))
		   lst))
    (if (> (length lst) 2)
	(list func
	      (op2 (cdr lst) func)
	      (car lst))
	(append (list func) lst)))
  (op2 (cdr prefix-expression) (car prefix-expression)))

(defun append-func (func list)
  (only-two-arg (append (list func) list))) 

(defun infix->prefix (infix-expr
		      &optional (separators *separators*)
			(test #'eql))
  (only-two-arg
   (infix->prefix-optimized infix-expr separators test)))


;;;  PREFIX->INFIX


(defun insert-between (lst sep)
  (if (or (not (consp lst))
	  (not (rest lst)))
      lst
      (cons (first lst) (mapcan #'(lambda (x) (list sep x)) (rest lst)))))

(defun prefix->infix (prefix-expr
		      &optional (separators *separators*)
			(test #'eql))
  "Converts a prefix expression to infix"
  (let ((in-expr
	 (mapcar #'(lambda (x) (remove-brackets
			   (if (and (listp x)
				    (not (equalp (car x) '¬))) 
			       (prefix->infix x separators)
			       x)))
		 prefix-expr)))
    (if (or (not (listp in-expr))
	    (not (member (first in-expr) separators :test test)))
	in-expr
	(insert-between (rest in-expr) (first in-expr)))))


;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

;;; Полином жегалкина (алгебраическая нормальная форма)

(defun anf-conversion (list)
  (loop for i to (1- (length list)) collect
       (let ((var (intern (string (char+ #\a i)))))
	 (if (nth i list) var (list '⊕ var t)))))

(defun anf-disjuncts (vector)
  (let ((table (boolean-table (log (length vector) 2 ))))
    (loop for i to (length vector) collect
	 (if (equalp (nth i vector) 1)
	     (append-func '∧ (anf-conversion (nth i table)))))))

(defun result-vector->anf (vector)
  (append-func '⊕ (remove 'nil (anf-disjuncts vector))))

(defun ->anf (func)
  (result-vector->anf (map 'list 'bool->int (result-vector func))))

;;; Дизъюнктивная нормальная форма

(defun pdnf-conversion (list)
  (loop for i to (1- (length list)) collect
       (let ((var (intern (string (char+ #\a i)))))
	 (if (nth i list) var (list '¬ var)))))

(defun pdnf-disjuncts (vector)
  (let ((table (boolean-table (log (length vector) 2 ))))
    (loop for i to (length vector) collect
	 (if (equalp (nth i vector) 1)
	     (append-func '∧ (pdnf-conversion (nth i table)))))))

(defun result-vector->perfect-dnf (vector)
  (append-func '∨ (remove 'nil (pdnf-disjuncts vector))))

(defun ->dnf (func)
  (result-vector->perfect-dnf
   (map 'list 'bool->int (result-vector func))))

;;; Конъюктивная нормальная форма

(defun pcnf-conversion (list)
  (loop for i to (1- (length list)) collect
       (let ((var (intern (string (char+ #\a i)))))
	 (if (nth i list) (list '¬ var) var))))

(defun pcnf-conjuncts (vector)
  (let ((table (boolean-table (log (length vector) 2 ))))
    (loop for i to (length vector) collect
	 (if (equalp (nth i vector) 0)
	     (append-func '∨ (pcnf-conversion (nth i table)))))))

(defun result-vector->perfect-cnf (vector)
  (append-func '∧ (remove 'nil (pcnf-conjuncts vector))))

(defun ->cnf (func)
  (result-vector->perfect-cnf
   (map 'list 'bool->int (result-vector func))))

;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun permute (n)
  (all-permutations (loop for i from 1 to n collect i)))

(defun rand-permute (n)
  (alexandria:shuffle (loop for i from 1 to n collect i)))

(defun mul-perms-assoc (zlist var)
  (if (not (null zlist))
      (let ((associated (assoc var zlist)))
	(append (list (car associated))
		(if (null associated)
		    (mul-perms-assoc zlist (caar zlist))
		    (mul-perms-assoc (remove associated zlist)
				     (cadr associated)))))))

(defun mul-perms (flist slist)
  (let ((zlist (mapcar 'list flist slist)))
    (remove nil (mul-perms-assoc zlist (caar zlist)))))

(defun perms-commutativityp (f s)
  (equalp (mul-perms f s)
	  (mul-perms s f)))

(defun perms-groupp (list)
  (let ((sorted-list (map 'list (lambda (x) (sort x '>)) list)))
    (equalp (count (car sorted-list) sorted-list :test 'equalp)
	    (length list))))

;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

;; Булевы структуры

(defun describe-boolean-cube (n)
  (let ((boolean-table (boolean-table n)))
    (loop for i to n do
	 (loop for j in boolean-table do
	      (if (equalp (count 't j) i)
		  (format t "~A " (boolean-list->binary-str j))))
	 (newline))))

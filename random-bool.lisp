;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package :cl-logic)

(defun random-bool-func (nvars &optional (form 'dnf))
  (if (< nvars 2) (error "nvars cannot less than 2"))
  (let ((vector (loop for i from 1 to (expt 2 nvars) collect (random 2))))
    (cond ((equalp form 'dnf) (result-vector->perfect-dnf vector))
	  ((equalp form 'cnf) (result-vector->perfect-cnf vector))
	  ((equalp form 'anf) (result-vector->anf vector))
	  (t (error "Sorry, form ~s not implemented." form)))))

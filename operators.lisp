;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

;;; Определение элементарных булевых функций

(defun ¬ (p) "Инверсия, uac" (not p))

(defun ∧ (p q) "Конъюнкция, u2227" (and p q))

(defun ∨ (p q) "Дизъюнкция, u2228" (or p q)) 

(defun → (p q) "Импликация, u2192" (or (not p) q))

(defun ⊕ (p q) "Сложение по модулю 2, u2295" (not (equal p q)))

(defun ∼ (p q) "Эквивалентность, u223c" (equal p q))

(defun ↑ (p q) "Штрих Шеффера, u2191" (not (and p q)))

(defun ↓ (p q) "Стрелка Пирса, u2193" (not (or p q)))

(defun help-operators ()
  (format t "¬~%∧~%∨~%→~%⊕~%∼~%↑~%↓~%")
  (format t "¬ \\uac~%")
  (format t "∧ \\u2227~%")
  (format t "∨ \\u2228~%")
  (format t "→ \\u2192~%")
  (format t "⊕ \\u2295~%")
  (format t "∼ \\u223c~%")
  (format t "↑ \\u2191~%")
  (format t "↓ \\u2193~%"))

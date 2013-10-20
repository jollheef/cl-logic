;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package #:cl-logic)

(defun boolean-table (nvars)
  "Генерирует таблицу значений для nvars переменных"
  (loop for i from 0 to (1- (expt 2 nvars))
     collect (append-to-length (int->bool-list i) nvars nil)))

(defun truth-table (function)
  "Генерирует таблицу истинности функции"
  (map 'list (lambda (args) (list args  (apply function args)))
       (boolean-table (nvars function))))

(defun result-vector (function)
  "Возвращает вектор значений функции"
  (map 'list 'cadr (truth-table function)))

(defun print-chars (n)
  "Выводит символы в виде x, y, z \dots"
  (loop for i from 0 to (1- n) do (format t "~c " (char+ #\a i))))

(defun truth-table-print (function)
  "Вывод таблицы истинности для функции"
  (print-chars (nvars function))
  (format t "   ~s~%" function)
  (loop for line in (truth-table function) do
       (map 'list (lambda (l) (format t "~d " (bool->int l))) (car line))
       (format t "-> ~d~%" (bool->int (cadr line)))))

(defun equivalent (functions)
  (apply 'equalp (map 'list (lambda (func) (result-vector func))
		      functions)))

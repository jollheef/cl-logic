;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(in-package :cl-logic)

;;; Вспомогательные функции

(defun int->bool (char)
  "Переводит из двоичной формы в булеву"
  (if (equalp (parse-integer (string char)) 0) nil t))

(defun nvars (function)
  "Количество переменных для функции"
  (length (sb-introspect:function-lambda-list function)))

(defun bool->int (bool)
  "Переводит из булевой формы в двоичную"
  (if bool 1 0))

(defun gen-list (obj n)
  "Генерирует список размера n из объект obj"
  (if (plusp n) (append (list obj) (gen-list obj (1- n)))))

(defun int->bool-list (int)
  "Переводит число из десятичной системы в список булевых"
  (map 'list (lambda (char) (int->bool char)) (format nil "~b" int)))

(defun append-to-length (list int obj)
  "Добавляет объект obj к списку list до длины int"
  (append (gen-list obj (- int (length list))) list))

(defun char+ (char &optional (n 1))
  "Возвращает следующий либо char+n символ в таблице"
  (code-char (+ (char-code char) n)))

(defun newline ()
  "Переход на новую строку"
  (format t "~%"))

(defun strcat (&rest strings)
  (apply 'concatenate 'string strings))

(defun boolean-list->binary-str (list)
  (apply 'strcat (loop for i in list collect
		      (if i "1" "0"))))

(defun int->bool-bin-list (n)
  (mapcar 'bool->int (int->bool-list n)))

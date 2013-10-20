;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(asdf:defsystem #:cl-logic
  :serial t
  :description "Boolean algebra package"
  :author "Mikhail Klementyev <jollheef@riseup.net>"
  :license "GNU GPLv3"
  :components ((:static-file "COPYING")
	       (:file "package")
	       (:file "cl-logic" :depends-on ("package"))
	       (:file "def" :depends-on ("package"))
	       (:file "dual" :depends-on ("package"))
	       (:file "infix" :depends-on ("package"))
	       (:file "operators" :depends-on ("package"))
	       (:file "shortcuts" :depends-on ("package"))
	       (:file "form" :depends-on ("package"))
	       (:file "bool-structs" :depends-on ("package"))))

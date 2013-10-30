;;;; This file is part of the cl-logic library, released under
;;;; GNU General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Klementyev Mikhail <jollheef@riseup.net>

(defpackage #:cl-logic
  (:use #:cl)
  (:export #:infix->prefix))

(defpackage #:quine-mccluskey
  (:nicknames "qm")
  (:use #:cl)
  (:export #:quine-mccluskey))

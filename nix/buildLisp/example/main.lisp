(defpackage example
  (:use :cl :lib-example)
  (:export :main))
(in-package :example)

(defun main ()
  (format t "i <3 ~S" (who)))

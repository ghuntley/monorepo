(defpackage #:dns
  (:documentation "Simple DNS resolver in Common Lisp")
  (:use #:cl #:iterate #:lisp-binary)
  (:export #:lookup-txt #:lookup-mx))

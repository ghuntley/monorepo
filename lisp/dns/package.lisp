(defpackage #:dns
  (:documentation "Simple DNS resolver in Common Lisp")
  (:use #:cl #:iterate #:lisp-binary)
  (:export
   ;; Individual lookup functions
   #:lookup-txt #:lookup-mx #:lookup-cname #:lookup-a #:lookup-ns

   ;; Useful accessors
   #:dns-message-header #:dns-message-answer #:dns-message-question
   #:dns-rr-name #:dns-rr-type #:dns-rr-ttl #:dns-rr-rdata
   #:dns-question-qname #:dns-question-qtype))

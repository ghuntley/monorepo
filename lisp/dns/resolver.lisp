;; Initial implementation is a simple client for
;; https://developers.google.com/speed/public-dns/docs/doh/json

(defpackage #:dns
  (:documentation "Simple DNS resolver in Common Lisp")
  (:use #:cl)
  (:export #:lookup-txt #:lookup-mx))

(defvar *doh-base-url* "https://dns.google/resolve"
  "Base URL of the service providing DNS-over-HTTP(S). Defaults to the
  Google-hosted API.")

(defun lookup-generic (name type)
  (multiple-value-bind (body)
      (drakma:http-request *doh-base-url*
                           :decode-content t
                           :want-stream t
                           :parameters `(("type" . ,type)
                                         ("name" . ,name)
                                         ("ct" . "application/x-javascript")))
    (cl-json:decode-json body)))

(defun lookup-txt (name)
  "Look up the TXT records at NAME."
  (lookup-generic name "TXT"))

(defun lookup-mx (name)
  "Look up the MX records at NAME."
  (lookup-generic name "MX"))

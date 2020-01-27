;; Implementation of a DoH-client, see RFC 8484 (DNS Queries over
;; HTTPS (DoH))

(in-package #:dns)

(defvar *doh-base-url* "https://dns.google/resolve"
  "Base URL of the service providing DNS-over-HTTP(S). Defaults to the
  Google-hosted API.")

(define-condition doh-error (error)
  ((query-name :initarg :query-name
               :reader doh-error-query-name
               :type string)
   (query-type :initarg :query-type
               :reader doh-error-query-type
               :type string)
   (doh-url :initarg :doh-url
            :reader doh-error-doh-url
            :type string)
   (status-code :initarg :status-code
                :reader doh-error-status-code
                :type integer)
   (response-body :initarg :response-body
                  :reader doh-error-response-body
                  :type (or nil (vector (unsigned-byte 8)) string)))

  (:report (lambda (condition stream)
             (let ((url (doh-error-doh-url condition))
                   (status (doh-error-status-code condition))
                   (body (doh-error-response-body condition)))
               (format stream "DoH service at '~A' responded with non-success (~A): ~%~%~A"
                       url status body)))))

(defun lookup-generic (name type doh-url)
  (multiple-value-bind (body status)
      (drakma:http-request doh-url
                           :decode-content t
                           ;; TODO(tazjin): Figure out why 'want-stream' doesn't work
                           :parameters `(("type" . ,type)
                                         ("name" . ,name)
                                         ("ct" . "application/dns-message")))
    (if (= 200 status)
        (read-binary 'dns-message (flexi-streams:make-in-memory-input-stream body))

        (restart-case (error 'doh-error
                             :query-name name
                             :query-type type
                             :doh-url doh-url
                             :status-code status
                             :response-body body)
          (call-with-other-name (new-name)
            :interactive (lambda () (list (the string (read))))
            :test (lambda (c) (typep c 'doh-error))
            (lookup-generic new-name type doh-url))

          (call-with-other-type (new-type)
            :interactive (lambda () (list (the string (read))))
            :test (lambda (c) (typep c 'doh-error))
            (lookup-generic name new-type doh-url))

          (call-with-other-url (new-url)
            :interactive (lambda () (list (the string (read))))
            :test (lambda (c) (typep c 'doh-error))
            (lookup-generic name type new-url))))))

(defun lookup-a (name &key (doh-url *doh-base-url*))
  "Look up the A records at NAME."
  (lookup-generic name "A" doh-url))

(defun lookup-txt (name &key (doh-url *doh-base-url*))
  "Look up the TXT records at NAME."
  (lookup-generic name "TXT" doh-url))

(defun lookup-mx (name &key (doh-url *doh-base-url*))
  "Look up the MX records at NAME."
  (lookup-generic name "MX" doh-url))

(defun lookup-cname (name &key (doh-url *doh-base-url*))
  "Look up the CNAME records at NAME."
  (lookup-generic name "CNAME" doh-url))

(defun lookup-ns (name &key (doh-url *doh-base-url*))
  "Look up the NS records at NAME."
  (lookup-generic name "NS" doh-url))

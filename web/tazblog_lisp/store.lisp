(defpackage #:tazblog/store
  (:documentation
   "This module implements fetching of individual blog entries from DNS.
   Yes, you read that correctly.

   Each blog post is stored as a set of records in a designated DNS
   zone. For the production blog, this zone is `blog.tazj.in.`.

   A top-level record at `_posts` contains a list of all published
   post IDs.

   For each of these post IDs, there is a record at `_meta.$postID`
   that contains the title and number of post chunks.

   For each post chunk, there is a record at `_$chunkID.$postID` that
   contains a base64-encoded post fragment.

   This module implements logic for assembling a post out of these
   fragments and caching it based on the TTL of its `_meta` record.")

  (:use #:cl #:dns #:iterate)
  (:import-from #:cl-base64 #:base64-string-to-string))
(in-package :tazblog/store)

;; TODO:
;;
;; - implement DNS caching

(defvar *tazblog-zone* ".blog.tazj.in."
  "DNS zone in which blog posts are persisted.")

(deftype entry-id () 'string)

(defun list-entries (&key (offset 0) (count 4) (zone *tazblog-zone*))
  "Retrieve COUNT entry IDs from ZONE at OFFSET."
  (let ((answers (lookup-txt (concatenate 'string "_posts" zone))))
    (map 'vector #'dns-rr-rdata (subseq answers offset (+ offset count)))))

(defun get-entry-meta (entry-id zone)
  (let* ((name (concatenate 'string "_meta." entry-id zone))
         (answer (lookup-txt name))
         (encoded (dns-rr-rdata (alexandria:first-elt answer)))
         (meta-json (base64-string-to-string encoded)))
    (json:decode-json-from-string meta-json)))

(defun base64-add-padding (string)
  "Adds padding to the base64-encoded STRING if required."
  (let ((rem (- 4 (mod (length string) 4))))
    (if (= 0 rem) string
        (format nil "~A~v@{~A~:*~}" string rem "="))))

(defun collect-entry-fragments (entry-id count zone)
  (let* ((fragments
           (iter (for i from 0 below count)
             (for name = (format nil "_~D.~A~A" i entry-id zone))
             (collect (alexandria:first-elt (lookup-txt name)))))
         (decoded (map 'list (lambda (f)
                               (base64-string-to-string
                                (base64-add-padding (dns-rr-rdata f))))
                       fragments)))
    (apply #'concatenate 'string decoded)))

(defstruct entry
  (id "" :type string)
  (title "" :type string)
  (content "" :type string)
  (date "" :type string))

(defun get-entry (entry-id &optional (zone *tazblog-zone*))
  "Retrieve the entry at ENTRY-ID from ZONE."
  (let* ((meta (get-entry-meta entry-id zone))
         (count (cdr (assoc :c meta)))
         (title (cdr (assoc :t meta)))
         (date (cdr (assoc :d meta)))
         (content (collect-entry-fragments entry-id count zone)))
    (make-entry :id entry-id
                :date date
                :title title
                :content content)))

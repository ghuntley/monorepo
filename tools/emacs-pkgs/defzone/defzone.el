;;; defzone.el --- Generate zone files from Elisp  -*- lexical-binding: t; -*-

(require 'dash)
(require 'dash-functional)
(require 's)

(defun record-to-record (zone record &optional subdomain)
  "Evaluate a record definition and turn it into a zone file
  record in ZONE, optionally prefixed with SUBDOMAIN."

  (cl-labels ((plist->alist (plist)
                            (when plist
                              (cons
                               (cons (car plist) (cadr plist))
                               (plist->alist (cddr plist))))))
    (let ((name (if subdomain (s-join "." (list subdomain zone)) zone)))
      (pcase record
        ;; SOA RDATA (RFC 1035; 3.3.13)
        ((and `(SOA . (,ttl . ,keys))
              (let (map (:mname mname) (:rname rname) (:serial serial)
                        (:refresh refresh) (:retry retry) (:expire expire)
                        (:minimum min))
                (plist->alist keys)))
         (if-let ((missing (-filter #'null (not (list mname rname serial
                                                      refresh retry expire min)))))
             (error "Missing fields in SOA record: %s" missing)
             (format "%s %s IN SOA %s %s %s %s %s %s %s"
                     name ttl mname rname serial refresh retry expire min)))

        (`(NS . (,ttl . ,targets))
         (->> targets
              (-map (lambda (target) (format "%s %s IN NS %s" name ttl target)))
              (s-join "\n")))

        (`(MX . (,ttl . ,pairs))
         (->> pairs
              (-map (-lambda ((preference . exchange))
                      (format "%s %s IN MX %s %s" name ttl preference exchange)))
              (s-join "\n")))

        (`(TXT ,ttl ,text) (format "%s %s IN TXT %s" name ttl (prin1-to-string text)))

        (`(A . (,ttl . ,ips))
         (->> ips
              (-map (lambda (ip) (format "%s %s IN A %s" name ttl ip)))
              (s-join "\n")))

        (`(CNAME ,ttl ,target) (format "%s %s IN CNAME %s" name ttl target))

        ((and `(,sub . ,records)
              (guard (stringp sub)))
         (s-join "\n" (-map (lambda (r) (record-to-record zone r sub)) records)))

        (_ (error "Invalid record definition: %s" record))))))

(defmacro defzone (fqdn &rest records)
  "Generate zone file for the zone at FQDN from a simple DSL."
  (declare (indent defun))

  `(s-join "\n" (-map (lambda (r) (record-to-record ,fqdn r)) (quote ,records))))

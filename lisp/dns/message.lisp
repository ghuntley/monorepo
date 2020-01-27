(in-package :dns)

;; 3.3. Standard RRs

;; The following RR definitions are expected to occur, at least
;; potentially, in all classes.  In particular, NS, SOA, CNAME, and PTR
;; will be used in all classes, and have the same format in all classes.
;; Because their RDATA format is known, all domain names in the RDATA
;; section of these RRs may be compressed.

;; <domain-name> is a domain name represented as a series of labels, and
;; terminated by a label with zero length.  <character-string> is a single
;; length octet followed by that number of characters.  <character-string>
;; is treated as binary information, and can be up to 256 characters in
;; length (including the length octet).


;; 3.3.11. NS RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     /                   NSDNAME                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; NSDNAME         A <domain-name> which specifies a host which should be
;;                 authoritative for the specified class and domain.

;; NS records cause both the usual additional section processing to locate
;; a type A record, and, when used in a referral, a special search of the
;; zone in which they reside for glue information.

;; The NS RR states that the named host should be expected to have a zone
;; starting at owner name of the specified class.  Note that the class may
;; not indicate the protocol family which should be used to communicate
;; with the host, although it is typically a strong hint.  For example,
;; hosts which are name servers for either Internet (IN) or Hesiod (HS)
;; class information are normally queried using IN class protocols.

;; 3.3.12. PTR RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     /                   PTRDNAME                    /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; PTRDNAME        A <domain-name> which points to some location in the
;;                 domain name space.

;; PTR records cause no additional section processing.  These RRs are used
;; in special domains to point to some other location in the domain space.
;; These records are simple data, and don't imply any special processing
;; similar to that performed by CNAME, which identifies aliases.  See the
;; description of the IN-ADDR.ARPA domain for an example.

;; 3.3.13. SOA RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     /                     MNAME                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     /                     RNAME                     /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    SERIAL                     |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    REFRESH                    |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     RETRY                     |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    EXPIRE                     |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    MINIMUM                    |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; MNAME           The <domain-name> of the name server that was the
;;                 original or primary source of data for this zone.

;; RNAME           A <domain-name> which specifies the mailbox of the
;;                 person responsible for this zone.

;; SERIAL          The unsigned 32 bit version number of the original copy
;;                 of the zone.  Zone transfers preserve this value.  This
;;                 value wraps and should be compared using sequence space
;;                 arithmetic.

;; REFRESH         A 32 bit time interval before the zone should be
;;                 refreshed.

;; RETRY           A 32 bit time interval that should elapse before a
;;                 failed refresh should be retried.

;; EXPIRE          A 32 bit time value that specifies the upper limit on
;;                 the time interval that can elapse before the zone is no
;;                 longer authoritative.

;; MINIMUM         The unsigned 32 bit minimum TTL field that should be
;;                 exported with any RR from this zone.

;; SOA records cause no additional section processing.

;; All times are in units of seconds.

;; Most of these fields are pertinent only for name server maintenance
;; operations.  However, MINIMUM is used in all query operations that
;; retrieve RRs from a zone.  Whenever a RR is sent in a response to a
;; query, the TTL field is set to the maximum of the TTL field from the RR
;; and the MINIMUM field in the appropriate SOA.  Thus MINIMUM is a lower
;; bound on the TTL field for all RRs in a zone.  Note that this use of
;; MINIMUM should occur when the RRs are copied into the response and not
;; when the zone is loaded from a master file or via a zone transfer.  The
;; reason for this provison is to allow future dynamic update facilities to
;; change the SOA RR with known semantics.


;; 3.3.14. TXT RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     /                   TXT-DATA                    /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; TXT-DATA

;; TXT RRs are used to hold descriptive text.  The semantics of the text
;; depends on the domain where it is found.

(defbinary dns-header (:byte-order :big-endian)
           ;; A 16 bit identifier assigned by the program that
           ;; generates any kind of query. This identifier is copied
           ;; the corresponding reply and can be used by the requester
           ;; to match up replies to outstanding queries.
           (id 0 :type 16)

           ;; A one bit field that specifies whether this message is a
           ;; query (0), or a response (1).
           (qr 0 :type 1)

           ;; A four bit field that specifies kind of query in this
           ;; message. This value is set by the originator of a query
           ;; and copied into the response. The values are:
           ;;
           ;; 0               a standard query (QUERY)
           ;; 1               an inverse query (IQUERY)
           ;; 2               a server status request (STATUS)
           ;; 3-15            reserved for future use
           (opcode 0 :type 4)

           ;; Authoritative Answer - this bit is valid in responses,
           ;; and specifies that the responding name server is an
           ;; authority for the domain name in question section.
           (aa nil :type 1)

           ;; TrunCation - specifies that this message was truncated
           ;; due to length greater than that permitted on the
           ;; transmission channel.
           (tc nil :type 1)

           ;; Recursion Desired - this bit may be set in a query and
           ;; is copied into the response.  If RD is set, it directs
           ;; the name server to pursue the query recursively.
           ;; Recursive query support is optional.
           (rd nil :type 1)

           ;; Recursion Available - this be is set or cleared in a
           ;; response, and denotes whether recursive query support is
           ;; available in the name server.
           (ra nil :type 1)

           ;; Reserved for future use. Must be zero in all queries and
           ;; responses.
           (z 0 :type 3)

           ;; Response code - this 4 bit field is set as part of
           ;; responses.  The values have the following
           ;; interpretation:
           ;; 0               No error condition
           ;; 1               Format error - The name server was
           ;;                 unable to interpret the query.
           ;; 2               Server failure - The name server was
           ;;                 unable to process this query due to a
           ;;                 problem with the name server.
           ;; 3               Name Error - Meaningful only for
           ;;                 responses from an authoritative name
           ;;                 server, this code signifies that the
           ;;                 domain name referenced in the query does
           ;;                 not exist.
           ;; 4               Not Implemented - The name server does
           ;;                 not support the requested kind of query.
           ;; 5               Refused - The name server refuses to
           ;;                 perform the specified operation for
           ;;                 policy reasons.  For example, a name
           ;;                 server may not wish to provide the
           ;;                 information to the particular requester,
           ;;                 or a name server may not wish to perform
           ;;                 a particular operation (e.g., zone
           ;;                 transfer) for particular data.
           ;; 6-15            Reserved for future use.
           (rcode 0 :type 4)

           ;; an unsigned 16 bit integer specifying the number of
           ;; entries in the question section.
           (qdcount 0 :type 16)

           ;; an unsigned 16 bit integer specifying the number of
           ;; resource records in the answer section.
           (ancount 0 :type 16)

           ;; an unsigned 16 bit integer specifying the number of name
           ;; server resource records in the authority records
           ;; section.
           (nscount 0 :type 16)

           ;; an unsigned 16 bit integer specifying the number of
           ;; resource records in the additional records section.
           (arcount 0 :type 16))


;; Representation of DNS QNAMEs.
;;
;; A QNAME can be either made up entirely of labels, which is
;; basically a list of strings, or be terminated with a pointer to an
;; offset within the original message.

(deftype qname-field ()
  '(or
    ;; pointer
    (unsigned-byte 14)
    ;; label
    string))

(defstruct qname
  (start-at 0 :type (unsigned-byte 14))
  (names #() :type (vector qname-field)))

;; Domain names in questions and resource records are represented as a
;; sequence of labels, where each label consists of a length octet
;; followed by that number of octets.
;;
;; The domain name terminates with the zero length octet for the null
;; label of the root. Note that this field may be an odd number of
;; octets; no padding is used.
(declaim (ftype (function (stream) (values qname integer)) read-qname))
(defun read-qname (stream)
  "Reads a DNS QNAME from STREAM."

  (let ((start-at (file-position stream)))
    (iter (for byte next (read-byte stream))
      ;; Each fragment is collected into this byte vector pre-allocated
      ;; with the correct size.
      (for fragment = (make-array byte :element-type '(unsigned-byte 8)
                                       :fill-pointer 0))

      ;; If the bit sequence (1 1) is encountered at the beginning of
      ;; the fragment, a qname pointer is being read.
      (let ((byte-copy byte))
        (when (equal #b11 (lisp-binary/integer:pop-bits 2 8 byte-copy))
          (let ((next (read-byte stream)))
            (lisp-binary/integer:push-bits byte-copy 8 next)
            (collect next into fragments result-type vector)
            (sum 2 into size)
            (finish))))

      ;; Total size is needed, count for each iteration byte, plus its
      ;; own value.
      (sum (+ 1 byte) into size)
      (until (equal byte 0))

      ;; On each iteration, this will interpret the current byte as an
      ;; unsigned integer and read from STREAM an equivalent amount of
      ;; times to assemble the current fragment.
      ;;
      ;; Advancing the stream like this also ensures that the next
      ;; iteration occurs on a new fragment or the final terminating
      ;; byte.
      (dotimes (_ byte (collect (babel:octets-to-string fragment)
                         into fragments result-type vector))
        (vector-push (read-byte stream) fragment))

      (finally (return (values (make-qname :start-at start-at
                                           :names fragments)
                               size))))))

(declaim (ftype (function (stream qname)) write-qname))
(defun write-qname (stream qname)
  "Write a DNS qname to STREAM."

  ;; Write each fragment starting with its (byte-) length, followed by
  ;; the bytes.
  (iter (for fragment in-vector (qname-names qname))
    (for bytes = (babel:string-to-octets fragment))
    (write-byte (length bytes) stream)
    (iter (for byte in-vector bytes)
      (write-byte byte stream)))

  ;; Always finish off the serialisation with a null-byte!
  (write-byte 0 stream))

(define-enum dns-type 2
    (:byte-order :big-endian)

    ;; http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml
    (A 1)
    (NS 2)
    (CNAME 5)
    (SOA 6)
    (PTR 12)
    (MX 15)
    (TXT 16)
    (SRV 33)
    (AAAA 28)

    ;; ANY typically wants SOA, MX, NS and MX
    (ANY 255))

(defbinary dns-question (:byte-order :big-endian :export t)
           ;; a domain name represented
           (qname "" :type (custom :lisp-type qname
                                   :reader #'read-qname
                                   :writer #'write-qname))

           ;; a two octet code which specifies the type of the query.
           (qtype 0 :type dns-type)

           ;; a two octet code that specifies the class of the query. For
           ;; example, the QCLASS field is IN for the Internet.
           (qclass 0 :type 16))

(defbinary dns-rr (:byte-order :big-endian :export t)
           (name nil :type (custom :lisp-type qname
                                   :reader #'read-qname
                                   :writer #'write-qname))

           ;; two octets containing one of the RR type codes. This field
           ;; specifies the meaning of the data in the RDATA field.
           (type 0 :type dns-type)

           ;; two octets which specify the class of the data in the RDATA
           ;; field.
           (class 0 :type 16)

           ;; a 32 bit unsigned integer that specifies the time interval (in
           ;; seconds) that the resource record may be cached before it should
           ;; be discarded. Zero values are interpreted to mean that the RR
           ;; can only be used for the transaction in progress, and should not
           ;; be cached.
           (ttl 0 :type 32)

           ;; an unsigned 16 bit integer that specifies the length in octets
           ;; of the RDATA field.
           (rdlength 0 :type 16)

           ;; a variable length string of octets that describes the resource.
           ;; The format of this information varies according to the TYPE and
           ;; CLASS of the resource record. For example, the if the TYPE is A
           ;; and the CLASS is IN, the RDATA field is a 4 octet ARPA Internet
           ;; address.
           (rdata #() :type (eval (case type
                                    ;; A 32-bit internet address in its
                                    ;; canonical representation of 4 integers.
                                    ((A) '(simple-array (unsigned-byte 8) (4)))

                                    ;; TODO(tazjin): Deal with multiple strings in single RRDATA
                                    ;; One or more <character-string>s.
                                    ((TXT) '(counted-string 1))

                                    ;; A <domain-name> which specifies the
                                    ;; canonical or primary name for the
                                    ;; owner. The owner name is an alias.
                                    ((CNAME) '(custom
                                               :lisp-type qname
                                               :reader #'read-qname
                                               :writer #'write-qname))

                                    ;; A <domain-name> which specifies a host
                                    ;; which should be authoritative for the
                                    ;; specified class and domain.
                                    ((NS) '(custom
                                            :lisp-type qname
                                            :reader #'read-qname
                                            :writer #'write-qname))
                                    (otherwise `(simple-array (unsigned-byte 8) (,rdlength)))))))

(defbinary dns-message (:byte-order :big-endian :export t)
           (header nil :type dns-header)

           ;; the question for the name server
           (question #() :type (simple-array dns-question ((dns-header-qdcount header))))

           ;; ;; RRs answering the question
           ;; (answer #() :type (simple-array (unsigned-byte 8) (16)))
           (answer #() :type (simple-array dns-rr ((dns-header-ancount header))))

           ;; ;; ;; RRs pointing toward an authority
           (authority #() :type (simple-array dns-rr ((dns-header-nscount header))))

           ;; ;; RRs holding additional information
           (additional #() :type (simple-array dns-rr ((dns-header-arcount header)))))

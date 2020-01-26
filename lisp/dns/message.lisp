(in-package :dns)

;; 3.2.2. TYPE values

;; TYPE fields are used in resource records.  Note that these types are a
;; subset of QTYPEs.

;; TYPE            value and meaning

;; A               1 a host address

;; NS              2 an authoritative name server

;; MD              3 a mail destination (Obsolete - use MX)

;; MF              4 a mail forwarder (Obsolete - use MX)

;; CNAME           5 the canonical name for an alias

;; SOA             6 marks the start of a zone of authority

;; MB              7 a mailbox domain name (EXPERIMENTAL)

;; MG              8 a mail group member (EXPERIMENTAL)

;; MR              9 a mail rename domain name (EXPERIMENTAL)

;; NULL            10 a null RR (EXPERIMENTAL)

;; WKS             11 a well known service description

;; PTR             12 a domain name pointer

;; HINFO           13 host information

;; MINFO           14 mailbox or mail list information

;; MX              15 mail exchange

;; TXT             16 text strings

;; 3.2.3. QTYPE values

;; QTYPE fields appear in the question part of a query.  QTYPES are a
;; superset of TYPEs, hence all TYPEs are valid QTYPEs.  In addition, the
;; following QTYPEs are defined:

;; AXFR            252 A request for a transfer of an entire zone

;; MAILB           253 A request for mailbox-related records (MB, MG or MR)

;; MAILA           254 A request for mail agent RRs (Obsolete - see MX)

;; *               255 A request for all records

;; 3.2.4. CLASS values

;; CLASS fields appear in resource records.  The following CLASS mnemonics
;; and values are defined:

;; IN              1 the Internet

;; CS              2 the CSNET class (Obsolete - used only for examples in
;;                 some obsolete RFCs)

;; CH              3 the CHAOS class

;; HS              4 Hesiod [Dyer 87]

;; 3.2.5. QCLASS values

;; QCLASS fields appear in the question section of a query.  QCLASS values
;; are a superset of CLASS values; every CLASS is a valid QCLASS.  In
;; addition to CLASS values, the following QCLASSes are defined:

;; *               255 any class

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

;; 3.3.1. CNAME RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     /                     CNAME                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; CNAME           A <domain-name> which specifies the canonical or primary
;;                 name for the owner.  The owner name is an alias.

;; CNAME RRs cause no additional section processing, but name servers may
;; choose to restart the query at the canonical name in certain cases.  See
;; the description of name server logic in [RFC-1034] for details.


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

;; TXT-DATA        One or more <character-string>s.

;; TXT RRs are used to hold descriptive text.  The semantics of the text
;; depends on the domain where it is found.

;; 3.4. Internet specific RRs

;; 3.4.1. A RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    ADDRESS                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; ADDRESS         A 32 bit Internet address.

;; Hosts that have multiple Internet addresses will have multiple A
;; records.


;; A records cause no additional section processing.  The RDATA section of
;; an A line in a master file is an Internet address expressed as four
;; decimal numbers separated by dots without any imbedded spaces (e.g.,
;; "10.2.0.52" or "192.0.5.6").

;; 3.4.2. WKS RDATA format

;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    ADDRESS                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |       PROTOCOL        |                       |
;;     +--+--+--+--+--+--+--+--+                       |
;;     |                                               |
;;     /                   <BIT MAP>                   /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

;; where:

;; ADDRESS         An 32 bit Internet address

;; PROTOCOL        An 8 bit IP protocol number

;; <BIT MAP>       A variable length bit map.  The bit map must be a
;;                 multiple of 8 bits long.

;; The WKS record is used to describe the well known services supported by
;; a particular protocol on a particular internet address.  The PROTOCOL
;; field specifies an IP protocol number, and the bit map has one bit per
;; port of the specified protocol.  The first bit corresponds to port 0,
;; the second to port 1, etc.  If the bit map does not include a bit for a
;; protocol of interest, that bit is assumed zero.  The appropriate values
;; and mnemonics for ports and protocols are specified in [RFC-1010].

;; For example, if PROTOCOL=TCP (6), the 26th bit corresponds to TCP port
;; 25 (SMTP).  If this bit is set, a SMTP server should be listening on TCP
;; port 25; if zero, SMTP service is not supported on the specified
;; address.

;; The purpose of WKS RRs is to provide availability information for
;; servers for TCP and UDP.  If a server supports both TCP and UDP, or has
;; multiple Internet addresses, then multiple WKS RRs are used.

;; WKS RRs cause no additional section processing.

;; In master files, both ports and protocols are expressed using mnemonics
;; or decimal numbers.

;; 3.5. IN-ADDR.ARPA domain

;; The Internet uses a special domain to support gateway location and
;; Internet address to host mapping.  Other classes may employ a similar
;; strategy in other domains.  The intent of this domain is to provide a
;; guaranteed method to perform host address to host name mapping, and to
;; facilitate queries to locate all gateways on a particular network in the
;; Internet.

;; Note that both of these services are similar to functions that could be
;; performed by inverse queries; the difference is that this part of the
;; domain name space is structured according to address, and hence can
;; guarantee that the appropriate data can be located without an exhaustive
;; search of the domain space.

;; The domain begins at IN-ADDR.ARPA and has a substructure which follows
;; the Internet addressing structure.

;; Domain names in the IN-ADDR.ARPA domain are defined to have up to four
;; labels in addition to the IN-ADDR.ARPA suffix.  Each label represents
;; one octet of an Internet address, and is expressed as a character string
;; for a decimal value in the range 0-255 (with leading zeros omitted
;; except in the case of a zero octet which is represented by a single
;; zero).

;; Host addresses are represented by domain names that have all four labels
;; specified.  Thus data for Internet address 10.2.0.52 is located at
;; domain name 52.0.2.10.IN-ADDR.ARPA.  The reversal, though awkward to
;; read, allows zones to be delegated which are exactly one network of
;; address space.  For example, 10.IN-ADDR.ARPA can be a zone containing
;; data for the ARPANET, while 26.IN-ADDR.ARPA can be a separate zone for
;; MILNET.  Address nodes are used to hold pointers to primary host names
;; in the normal domain space.

;; Network numbers correspond to some non-terminal nodes at various depths
;; in the IN-ADDR.ARPA domain, since Internet network numbers are either 1,
;; 2, or 3 octets.  Network nodes are used to hold pointers to the primary
;; host names of gateways attached to that network.  Since a gateway is, by
;; definition, on more than one network, it will typically have two or more
;; network nodes which point at it.  Gateways will also have host level
;; pointers at their fully qualified addresses.

;; Both the gateway pointers at network nodes and the normal host pointers
;; at full address nodes use the PTR RR to point back to the primary domain
;; names of the corresponding hosts.

;; For example, the IN-ADDR.ARPA domain will contain information about the
;; ISI gateway between net 10 and 26, an MIT gateway from net 10 to MIT's

;; net 18, and hosts A.ISI.EDU and MULTICS.MIT.EDU.  Assuming that ISI
;; gateway has addresses 10.2.0.22 and 26.0.0.103, and a name MILNET-
;; GW.ISI.EDU, and the MIT gateway has addresses 10.0.0.77 and 18.10.0.4
;; and a name GW.LCS.MIT.EDU, the domain database would contain:

;;     10.IN-ADDR.ARPA.           PTR MILNET-GW.ISI.EDU.
;;     10.IN-ADDR.ARPA.           PTR GW.LCS.MIT.EDU.
;;     18.IN-ADDR.ARPA.           PTR GW.LCS.MIT.EDU.
;;     26.IN-ADDR.ARPA.           PTR MILNET-GW.ISI.EDU.
;;     22.0.2.10.IN-ADDR.ARPA.    PTR MILNET-GW.ISI.EDU.
;;     103.0.0.26.IN-ADDR.ARPA.   PTR MILNET-GW.ISI.EDU.
;;     77.0.0.10.IN-ADDR.ARPA.    PTR GW.LCS.MIT.EDU.
;;     4.0.10.18.IN-ADDR.ARPA.    PTR GW.LCS.MIT.EDU.
;;     103.0.3.26.IN-ADDR.ARPA.   PTR A.ISI.EDU.
;;     6.0.0.10.IN-ADDR.ARPA.     PTR MULTICS.MIT.EDU.

;; Thus a program which wanted to locate gateways on net 10 would originate
;; a query of the form QTYPE=PTR, QCLASS=IN, QNAME=10.IN-ADDR.ARPA.  It
;; would receive two RRs in response:

;;     10.IN-ADDR.ARPA.           PTR MILNET-GW.ISI.EDU.
;;     10.IN-ADDR.ARPA.           PTR GW.LCS.MIT.EDU.

;; The program could then originate QTYPE=A, QCLASS=IN queries for MILNET-
;; GW.ISI.EDU. and GW.LCS.MIT.EDU. to discover the Internet addresses of
;; these gateways.

;; A resolver which wanted to find the host name corresponding to Internet
;; host address 10.0.0.6 would pursue a query of the form QTYPE=PTR,
;; QCLASS=IN, QNAME=6.0.0.10.IN-ADDR.ARPA, and would receive:

;;     6.0.0.10.IN-ADDR.ARPA.     PTR MULTICS.MIT.EDU.

;; Several cautions apply to the use of these services:
;;    - Since the IN-ADDR.ARPA special domain and the normal domain
;;      for a particular host or gateway will be in different zones,
;;      the possibility exists that that the data may be inconsistent.

;;    - Gateways will often have two names in separate domains, only
;;      one of which can be primary.

;;    - Systems that use the domain database to initialize their
;;      routing tables must start with enough gateway information to
;;      guarantee that they can access the appropriate name server.

;;    - The gateway data only reflects the existence of a gateway in a
;;      manner equivalent to the current HOSTS.TXT file.  It doesn't
;;      replace the dynamic availability information from GGP or EGP.

;; 3.6. Defining new types, classes, and special namespaces

;; The previously defined types and classes are the ones in use as of the
;; date of this memo.  New definitions should be expected.  This section
;; makes some recommendations to designers considering additions to the
;; existing facilities.  The mailing list NAMEDROPPERS@SRI-NIC.ARPA is the
;; forum where general discussion of design issues takes place.

;; In general, a new type is appropriate when new information is to be
;; added to the database about an existing object, or we need new data
;; formats for some totally new object.  Designers should attempt to define
;; types and their RDATA formats that are generally applicable to all
;; classes, and which avoid duplication of information.  New classes are
;; appropriate when the DNS is to be used for a new protocol, etc which
;; requires new class-specific data formats, or when a copy of the existing
;; name space is desired, but a separate management domain is necessary.

;; New types and classes need mnemonics for master files; the format of the
;; master files requires that the mnemonics for type and class be disjoint.

;; TYPE and CLASS values must be a proper subset of QTYPEs and QCLASSes
;; respectively.

;; The present system uses multiple RRs to represent multiple values of a
;; type rather than storing multiple values in the RDATA section of a
;; single RR.  This is less efficient for most applications, but does keep
;; RRs shorter.  The multiple RRs assumption is incorporated in some
;; experimental work on dynamic update methods.

;; The present system attempts to minimize the duplication of data in the
;; database in order to insure consistency.  Thus, in order to find the
;; address of the host for a mail exchange, you map the mail domain name to
;; a host name, then the host name to addresses, rather than a direct
;; mapping to host address.  This approach is preferred because it avoids
;; the opportunity for inconsistency.

;; In defining a new type of data, multiple RR types should not be used to
;; create an ordering between entries or express different formats for
;; equivalent bindings, instead this information should be carried in the
;; body of the RR and a single type used.  This policy avoids problems with
;; caching multiple types and defining QTYPEs to match multiple types.

;; For example, the original form of mail exchange binding used two RR
;; types one to represent a "closer" exchange (MD) and one to represent a
;; "less close" exchange (MF).  The difficulty is that the presence of one
;; RR type in a cache doesn't convey any information about the other
;; because the query which acquired the cached information might have used
;; a QTYPE of MF, MD, or MAILA (which matched both).  The redesigned




;; service used a single type (MX) with a "preference" value in the RDATA
;; section which can order different RRs.  However, if any MX RRs are found
;; in the cache, then all should be there.

;; 4. MESSAGES

;; 4.1. Format

;; All communications inside of the domain protocol are carried in a single
;; format called a message.  The top level format of message is divided
;; into 5 sections (some of which are empty in certain cases) shown below:

;; The names of the sections after the header are derived from their use in
;; standard queries.  The question section contains fields that describe a
;; question to a name server.  These fields are a query type (QTYPE), a
;; query class (QCLASS), and a query domain name (QNAME).  The last three
;; sections have the same format: a possibly empty list of concatenated
;; resource records (RRs).  The answer section contains RRs that answer the
;; question; the authority section contains RRs that point toward an
;; authoritative name server; the additional records section contains RRs
;; which relate to the query, but are not strictly answers for the
;; question.

;; The header section is always present.  The header includes fields that
;; specify which of the remaining sections are present, and also specify
;; whether the message is a query or a response, a standard query or some
;; other opcode, etc.

;; 4.1.1. Header section format

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
           (opcode 0 :type 4) ; TODO(tazjin): use define-enum

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
      ;;
      ;; TODO(tazjin): Use lisp-binary:read-counted-string.
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

;; 4.1.2. Question section format
(defbinary dns-question (:byte-order :big-endian)
           ;; a domain name represented
           (qname "" :type (custom :lisp-type qname
                                   :reader #'read-qname
                                   :writer #'write-qname))

           ;; a two octet code which specifies the type of the query.
           ;; The values for this field include all codes valid for a
           ;; TYPE field, together with some more general codes which
           ;; can match more than one type of RR.
           (qtype 0 :type 16) ;; TODO(tazjin): define type after the RR binary

           ;; a two octet code that specifies the class of the query. For
           ;; example, the QCLASS field is IN for the Internet.
           (qclass 0 :type 16)) ; TODO(tazjin): enum?

;; 4.1.3. Resource record format

(defbinary dns-rr (:byte-order :big-endian)
           (name nil :type (custom :lisp-type qname
                                   :reader #'read-qname
                                   :writer #'write-qname))

           ;; two octets containing one of the RR type codes. This
           ;; field specifies the meaning of the data in the RDATA
           ;; field.
           (type 0 :type 16)            ; TODO(tazjin): enum?

           ;; two octets which specify the class of the data in the
           ;; RDATA field.
           (class 0 :type 16)           ; TODO(tazjin): enum

           ;; a 32 bit unsigned integer that specifies the time
           ;; interval (in seconds) that the resource record may be
           ;; cached before it should be discarded. Zero values are
           ;; interpreted to mean that the RR can only be used for the
           ;; transaction in progress, and should not be cached.
           (ttl 0 :type 32)

           ;; an unsigned 16 bit integer that specifies the length in
           ;; octets of the RDATA field.
           (rdlength 0 :type 16)

           ;; a variable length string of octets that describes the
           ;; resource. The format of this information varies
           ;; according to the TYPE and CLASS of the resource record.
           ;; For example, the if the TYPE is A and the CLASS is IN,
           ;; the RDATA field is a 4 octet ARPA Internet address.
           (rdata #() :type (simple-array (unsigned-byte 8) (rdlength))))

(defbinary dns-message (:byte-order :big-endian)
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

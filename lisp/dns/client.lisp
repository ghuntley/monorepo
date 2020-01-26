;; Implementation of a DoH-client, see RFC 8484 (DNS Queries over
;; HTTPS (DoH))

(in-package #:dns)

;;    The DoH client is configured with a URI Template [RFC6570]
(defvar *doh-base-url* "https://dns.google/dns-query"
  "Base URL of the service providing DNS-over-HTTP(S). Defaults to the
  Google-hosted API.")

(defun lookup-generic (name type)
  (multiple-value-bind (stream)
      (drakma:http-request *doh-base-url*
                           :decode-content t
                           :want-stream t
                           :parameters `(("type" . ,type)
                                         ("name" . ,name)
                                         ("ct" . "application/dns-message")))
    (read-binary 'dns-message stream)))

(defun lookup-txt (name)
  "Look up the TXT records at NAME."
  (lookup-generic name "TXT"))

(defun lookup-mx (name)
  "Look up the MX records at NAME."
  (lookup-generic name "MX"))


;;    The URI Template defined in this document is processed without any
;;    variables when the HTTP method is POST.  When the HTTP method is GET,
;;    the single variable "dns" is defined as the content of the DNS
;;    request (as described in Section 6), encoded with base64url
;;    [RFC4648].

;;    When using the POST method, the DNS query is included as the message
;;    body of the HTTP request, and the Content-Type request header field
;;    indicates the media type of the message.  POSTed requests are
;;    generally smaller than their GET equivalents.

;;    Using the GET method is friendlier to many HTTP cache
;;    implementations.

;;    The DoH client SHOULD include an HTTP Accept request header field to
;;    indicate what type of content can be understood in response.
;;    Irrespective of the value of the Accept request header field, the
;;    client MUST be prepared to process "application/dns-message" (as
;;    described in Section 6) responses but MAY also process other DNS-
;;    related media types it receives.

;;    In order to maximize HTTP cache friendliness, DoH clients using media
;;    formats that include the ID field from the DNS message header, such
;;    as "application/dns-message", SHOULD use a DNS ID of 0 in every DNS
;;    request.  HTTP correlates the request and response, thus eliminating
;;    the need for the ID in a media type such as "application/dns-
;;    message".  The use of a varying DNS ID can cause semantically
;;    equivalent DNS queries to be cached separately.

;;    DoH clients can use HTTP/2 padding and compression [RFC7540] in the
;;    same way that other HTTP/2 clients use (or don't use) them.

;; 4.1.1.  HTTP Request Examples

;;    These examples use HTTP/2-style formatting from [RFC7540].

;;    These examples use a DoH service with a URI Template of
;;    "https://dnsserver.example.net/dns-query{?dns}" to resolve IN A
;;    records.

;;    The requests are represented as bodies with media type "application/
;;    dns-message".

;;    The first example request uses GET to request "www.example.com".

;;    :method = GET
;;    :scheme = https
;;    :authority = dnsserver.example.net
;;    :path = /dns-query?dns=AAABAAABAAAAAAAAA3d3dwdleGFtcGxlA2NvbQAAAQAB
;;    accept = application/dns-message

;;    Finally, a GET-based query for "a.62characterlabel-makes-base64url-
;;    distinct-from-standard-base64.example.com" is shown as an example to
;;    emphasize that the encoding alphabet of base64url is different than
;;    regular base64 and that padding is omitted.

;;    The only response type defined in this document is "application/dns-
;;    message", but it is possible that other response formats will be
;;    defined in the future.  A DoH server MUST be able to process
;;    "application/dns-message" request messages.

;;    Each DNS request-response pair is mapped to one HTTP exchange.

;;    DNS response codes indicate either success or failure for the DNS
;;    query.  A successful HTTP response with a 2xx status code (see
;;    Section 6.3 of [RFC7231]) is used for any valid DNS response,

;;    HTTP responses with non-successful HTTP status codes do not contain
;;    replies to the original DNS question in the HTTP request.  DoH
;;    clients need to use the same semantic processing of non-successful
;;    HTTP status codes as other HTTP clients.

;; 4.2.2.  HTTP Response Example

;;    This is an example response for a query for the IN AAAA records for
;;    "www.example.com" with recursion turned on.  The response bears one
;;    answer record with an address of 2001:db8:abcd:12:1:2:3:4 and a TTL
;;    of 3709 seconds.

;;    :status = 200
;;    content-type = application/dns-message
;;    content-length = 61
;;    cache-control = max-age=3709

;;    <61 bytes represented by the following hex encoding>
;;    00 00 81 80 00 01 00 01  00 00 00 00 03 77 77 77
;;    07 65 78 61 6d 70 6c 65  03 63 6f 6d 00 00 1c 00
;;    01 c0 0c 00 1c 00 01 00  00 0e 7d 00 10 20 01 0d
;;    b8 ab cd 00 12 00 01 00  02 00 03 00 04

;;    This protocol MUST be used with the https URI scheme [RFC7230].

;;    In particular, DoH servers SHOULD assign an explicit HTTP freshness
;;    lifetime (see Section 4.2 of [RFC7234]) so that the DoH client is
;;    more likely to use fresh DNS data.  This requirement is due to HTTP
;;    caches being able to assign their own heuristic freshness (such as
;;    that described in Section 4.2.2 of [RFC7234]), which would take
;;    control of the cache contents out of the hands of the DoH server.


;;    The assigned freshness lifetime of a DoH HTTP response MUST be less
;;    than or equal to the smallest TTL in the Answer section of the DNS
;;    response.  A freshness lifetime equal to the smallest TTL in the
;;    Answer section is RECOMMENDED.  For example, if a HTTP response
;;    carries three RRsets with TTLs of 30, 600, and 300, the HTTP
;;    freshness lifetime should be 30 seconds (which could be specified as
;;    "Cache-Control: max-age=30").  This requirement helps prevent expired
;;    RRsets in messages in an HTTP cache from unintentionally being
;;    served.

;;    If the DNS response has no records in the Answer section, and the DNS
;;    response has an SOA record in the Authority section, the response
;;    freshness lifetime MUST NOT be greater than the MINIMUM field from
;;    that SOA record (see [RFC2308]).

;;    DoH clients MUST account for the Age response header field's value
;;    [RFC7234] when calculating the DNS TTL of a response.  For example,
;;    if an RRset is received with a DNS TTL of 600, but the Age header
;;    field indicates that the response has been cached for 250 seconds,
;;    the remaining lifetime of the RRset is 350 seconds.  This requirement
;;    applies to both DoH client HTTP caches and DoH client DNS caches.

;;    Those features were introduced to HTTP in HTTP/2 [RFC7540].
;;    Earlier versions of HTTP are capable of conveying the semantic
;;    requirements of DoH but may result in very poor performance.

;;    In order to maximize interoperability, DoH clients and DoH servers
;;    MUST support the "application/dns-message" media type.

;;    The data payload for the "application/dns-message" media type is a
;;    single message of the DNS on-the-wire format defined in Section 4.2.1
;;    of [RFC1035], which in turn refers to the full wire format defined in
;;    Section 4.1 of that RFC.

;;    This media type restricts the maximum size of the DNS message to
;;    65535 bytes.

;;    When using the GET method, the data payload for this media type MUST
;;    be encoded with base64url [RFC4648] and then provided as a variable
;;    named "dns" to the URI Template expansion.  Padding characters for
;;    base64url MUST NOT be included.

;;    When using the POST method, the data payload for this media type MUST
;;    NOT be encoded and is used directly as the HTTP message body.

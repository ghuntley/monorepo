dns
===

This library is a DNS-over-HTTPS client for Common Lisp.

The ambition is to transform it into a fully-featured DNS resolver
instead of piggy-backing on the HTTPS implementation, but ...
baby-steps!

Note that there is no Common Lisp HTTP client that fully supports the
HTTP2 protocol at the moment, so you can not expect this library to
provide equivalent performance to a native DNS resolver (yet).

## API

The API is kept as simple as it can be.

### Types

The types of this library are implemented as several structs that
support binary (de-)serialisation via [lisp-binary][].

The existing structs are as follows and directly implement the
corresponding definitions from [RFC 1035][]:

* `dns-header`
* `dns-question`
* `dns-rr`
* `dns-message`

All relevant field accessors for these structs are exported and can be
used to inspect query results.

### Functions

All lookup functions are of the type `(function (string &key doh-url)
(dns-message))` and signal a `dns:doh-error` condition for
unsuccessful requests.

If `:doh-url` is unspecified, Google's public DNS-over-HTTPS servers
at [dns.google][https://dns.google] will be used.

Currently implemented lookup functions:

* `lookup-a`
* `lookup-mx`
* `lookup-txt`

## Example usage

```lisp
DNS> (dns-message-answer (lookup-a "git.tazj.in."))
#(#S(DNS-RR
     :NAME #S(QNAME :START-AT 29 :NAMES #(12))
     :TYPE A
     :CLASS 1
     :TTL 286
     :RDLENGTH 4
     :RDATA #(34 98 120 189)))
```

## TODO

Various things in this library are currently broken because I only
implemented it to work for my blog setup, but these things will be
ironed out.

Most importantly, the following needs to be fixed:

* Each qname *fragment* needs to track its offset, not each qname.
* The RDATA for a TXT record can have multiple counted strings.
* qnames should be canonicalised after parsing.

[lisp-binary]: https://github.com/j3pic/lisp-binary
[RFC 1035]: https://tools.ietf.org/html/rfc1035

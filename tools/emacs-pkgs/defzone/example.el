;;; example.el - usage example for defzone macro

(defzone "tazj.in."
  (SOA 21600
       :mname "ns-cloud-a1.googledomains.com."
       :rname "cloud-dns-hostmaster.google.com."
       :serial 123
       :refresh 21600
       :retry 3600
       :expire 1209600
       :minimum 300)

  (NS 21600
      "ns-cloud-a1.googledomains.com."
      "ns-cloud-a2.googledomains.com."
      "ns-cloud-a3.googledomains.com."
      "ns-cloud-a4.googledomains.com.")

  (MX 300
      (1  . "aspmx.l.google.com.")
      (5  . "alt1.aspmx.l.google.com.")
      (5  . "alt2.aspmx.l.google.com.")
      (10 . "alt3.aspmx.l.google.com.")
      (10 . "alt4.aspmx.l.google.com."))

  (TXT 3600 "google-site-verification=d3_MI1OwD6q2OT42Vvh0I9w2u3Q5KFBu-PieNUE1Fig")

  (A 300 "34.98.120.189")

  ;; Nested record sets are indicated by a list that starts with a
  ;; string (this is just joined, so you can nest multiple levels at
  ;; once)
  ("blog"
   ;; Blog "storage engine" is in a separate DNS zone
   (NS 21600
       "ns-cloud-c1.googledomains.com."
       "ns-cloud-c2.googledomains.com."
       "ns-cloud-c3.googledomains.com."
       "ns-cloud-c4.googledomains.com."))

  ("git"
   (A 300 "34.98.120.189")
   (TXT 300 "<3 edef"))

  ("files" (CNAME 300 "c.storage.googleapis.com.")))
